<?php
class eBankClass
{
    private $testando = true;

    /**
     * Token de autenticação para a API do eBank.
     */
	private $token;
    private $tokenType;
    private $tokenExpiracao; // data de expiração do token, em timestamp

    /**
     * Parâmetros para o consumo da API do eBank.
     */
    private $appId = "<<SEU APP ID>>"; // appId do eBank
    private $secret = "<<SEU SECRET>>"; // secret do eBank
    private $configurationId = '<<SEU CONFIGURATIONID>>'; // ID da configuração do eBank
    private $endpoint;
    private $method;
    private $statusCode;
    private $ebankResponse = [ ];
    private $erro = '';
	
    /**
     * Registra um boleto no banco
     * 
     * @param string $jsonText - JSON com os dados do boleto a ser registrado.
     * @return bool
     */
    public function registraBoleto($jsonText)
    {
        if (!$this->autentica()) {
            return false;
        }

        // enviar para a API
        $this->endpoint = "https://unimake.app/ebank/api/v1/boleto/registrar?configurationId={$this->configurationId}";

        return $this->enviaParaAPI($jsonText);
    }

    /**
     * Cancelar o boleto no banco
     * 
     * @param string $jsonText - JSON com os dados do boleto a ser cancelado
     * @return bool
     */
    public function cancelaBoleto($jsonText)
    {
        if (!$this->autentica()) {
            return false;
        }

        $this->endpoint = "https://unimake.app/ebank/api/v1/boleto/cancelar?configurationId={$this->configurationId}";

        return  $this->enviaParaAPI($jsonText);
    }

    /**
     * Alterar o vencimento do boleto no banco
     * 
     * @param string $jsonText - JSON com os dados do boleto a ser alterado
     * @return bool
     */
    public function alteraVencimentoBoleto($jsonText)
    {
        if (!$this->autentica()) {
            return false;
        }

        $this->endpoint = "https://unimake.app/ebank/api/v1/boleto/enviarinstrucao?configurationId={$this->configurationId}";
        //$this->endpoint = "https://unimake.app/ebank/api/v1/boleto/alterarvencimento?configurationId={$this->configurationId}";

        return $this->enviaParaAPI($jsonText);
    }

    /**
     * Conceder ou altera o desconto a um boleto já registrado no banco.
     * 
     * @param string $jsonText - JSON com os dados do boleto a ser alterado
     * @return bool
     */
    public function concedeDescontoBoleto($jsonText)
    {
        if (!$this->autentica()) {
            return false;
        }

        $this->endpoint = "https://unimake.app/ebank/api/v1/boleto/enviarinstrucao?configurationId={$this->configurationId}";

        return $this->enviaParaAPI($jsonText);
    }

    /**
     * Exclui o desconto de um boleto já registrado no banco
     * 
     * @param string $jsonText - JSON com os dados do boleto a ser alterado
     * @return bool
     */
    public function cancelaDescontoBoleto($jsonText)
    {
        if (!$this->autentica()) {
            return false;
        }

        $this->endpoint = "https://unimake.app/ebank/api/v1/boleto/enviarinstrucao?configurationId={$this->configurationId}";

        return $this->enviaParaAPI($jsonText);
    }


    /**
	* Autenticar no servidor de API do eBank.
	* A autenticação consiste, nessa classe, em ter um token válido, ainda não expirado, que usaremos para as transações.
    * @return bool
	*/	
    private function autentica()
    {
        // primeiro, verificar se já temos um token, e se ele não está expirado
        if(!empty($this->token) && !empty($this->tokenType) && $this->tokenExpiracao > time()) {
            return true;
        }

        // Verificar no banco de dados se já existe um token válido (não expirado).
        // Se existir, atribuir o token, o tipo e a expiração às propriedades da classe.
        // Exemplo de como pegar o token do banco (descomente a linha abaixo e implemente o método carregaTokenValidoDoBanco):
        // if($this->carregaTokenValidoDoBanco()) { // busca no banco se já existe um token válido, e se sim, atribui às propriedades
        //     return true;
        // }

        // Solicita um novo token
        return $this->solicitaNovoToken();
	} 
    
    /**
     * Solicita um novo token para autenticação no eBank
     * @return bool
     */
	private function solicitaNovoToken() 
	{
        $this->endpoint = "https://unimake.app/auth/api/auth/";        
        $this->method = 'post';
        $this->token = '';
        $this->tokenType = '';

        $base = [];
        $base["appId"] = $this->appId; // appId do eBank
        $base["secret"] = $this->secret; // secret 

        if($this->enviaParaAPI( json_encode($base) )) {

            // pegar o token e salvar na base de dados
            $expiracaoMS = $this->getResponse("expiration");
            $token       = $this->getResponse("token");
            $tipo        = $this->getResponse("type");

            if($expiracaoMS > 100 && strlen($token) > 30 && ! empty($tipo)) {

				// propriedade expiração está em milisegundos. Converter para minutos, e descontar 1 minuto para evitar usar token expirado
				$expiracaoMinutos = intval($expiracaoMS / 1000 / 60) - 1;

                // converte a expiração para uma data/hora do php, para posterior validação no método eBankAutentica
                $this->tokenExpiracao = time() + ($expiracaoMinutos * 60);

                // atribui o token à propriedade da classe
                $this->token = $token;
                $this->tokenType = $tipo;

                return true;
			}
        }

        // se chegou até aqui não conseguiu pegar o token
        return false;
    }

    /**
     * Enviar uma requisição para a API do eBank
     * @param string $jsonText = jason que vai ser enviado.
     * @return bool 
     */
    private function enviaParaAPI($jsonText)
    {
        // Alterar o endpoint quando estiver em ambiente de homologação (testes)
        if($this->testando) {
            if(strpos($this->endpoint, '/auth/') === false) {
                $this->endpoint = str_replace("https://unimake.app/ebank", "https://ebank.sandbox.unimake.software", $this->endpoint);
            } else {
                $this->endpoint = str_replace("https://unimake.app/auth", "https://auth.sandbox.unimake.software", $this->endpoint);
            }
        }

        // zera a propriedade "erro"
        $this->setErro('');

        // headers que serão enviados para a API
        $header = [];
        $header[] = 'Content-Type: application/json';
        $header[] = 'Accept: application/json';

        // adicionar o token ao header
        if(!empty($this->token)) {
            $header[] = "Authorization: {$this->tokenType} {$this->token}";
        }

        // Cria a conexão CURL com a URL e envia os dados
        $ch = curl_init();

        switch (strtolower($this->method)) {
            case 'post':
                curl_setopt($ch, CURLOPT_POST, 1);
                curl_setopt($ch, CURLOPT_POSTFIELDS, $jsonText);
                break;
        }

        $ua  = 'Unifake Software (unifake@unifake.com.br)';

        curl_setopt($ch, CURLOPT_URL, $this->endpoint);
        curl_setopt($ch, CURLOPT_HEADER, true);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_USERAGENT, $ua);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $header);

        // Executa a url        
        $response   = curl_exec($ch);
        $curl_errno = curl_errno($ch);
        $curl_error = curl_error($ch);

        // Trata erros/respostas
        $this->statusCode     = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        $headersize           = curl_getinfo($ch, CURLINFO_HEADER_SIZE);

        // Fecha a conexão CURL
        curl_close($ch);

        // Tratar a resposta, separando o header do body (que é um json)
        $jsonResposta = substr($response, $headersize);

        // Remover espaços à esqueda da primeira chave, se houver
        $pos = strpos($jsonResposta, '{');
        if ($pos > 0) {
            $jsonResposta = substr($jsonResposta, $pos);
        }

        // Salvar o json em um arquivo, para depuração
        /*
        if (strpos($this->endpoint, '/registrar') !== false) { // alterar essa condição se necessário
            $fileName = "response-" . date("Y-m-d_H-i-s") . strval(rand(1000, 9999)) . '.json';
            echo "--- ($fileName) <br>";
            file_put_contents($fileName, $jsonResposta);
        }
        */

        // Cria um array a partir do json recebido, e o deixa disponível para leituras posteriores no método chamador.
        $this->ebankResponse = json_decode($jsonResposta, true);

        // Analisa se a requisição  foi bem sucedida
        if ($this->statusCode >= 200 && $this->statusCode <= 299) {
            return true;
        }

        //
        // Chegou aqui? bad news: deu erro!
        //

        // montar a string com o erro 
        $mensagemErro = $this->getResponse("title");        
        
        // anexar o traceId à mensagem. É importante para o DEV Unimake identificar problemas.
        $traceId = $this->getResponse("traceId");
        if (!empty($traceId)) {
            $mensagemErro .= ", traceId: $traceId";
        }

        // tentar pegar o erro detalhado, se estiver no json de resposta
        $pos1 = strpos($response, '"errors":{');
        if($pos1 !== false) {
            $texto = substr($response, $pos1+10);
            $pos2 = strpos($texto, '}');
            if($pos2 !== false) {
                $texto = substr($texto, 0, $pos2);
                $mensagemErro .= ", $texto";
            }
        }

        //
        // monta o erro completo (maior número de informações possível para identificar o problema)
        // 

        if (!empty($mensagemErro)) {
            $errors0 = $this->getResponse("errors")[0];
            $erroCompleto = 'Erro: "' . $mensagemErro . '", código: ' . $this->statusCode . (empty($errors0) ? "" : ", detalhes: $errors0");

        } else if(intval($curl_errno) > 0) { // Verifica se ocorreu um erro de CURL quando nenhum outro erro apareceu. 
            $this->statusCode = $curl_errno;
            $erroCompleto = 'Erro: "' . $curl_error . '", código: ' . $curl_errno;
        } else {
            $erroCompleto = 'Erro desconhecido ao acessar ' . $this->endpoint;
        }

        // gerar o erro
        $this->setErro($erroCompleto);

        return false;
    }  

    /**
     * GET uma resposta do eBank
     * @param string $key - chave do array de resposta
     * @return mixed - valor da chave ou null se não existir
     */
    public function getResponse($key)
    {
        return $this->ebankResponse[$key];
    }

    /**
     * SET erro durante acesso à API do eBank
     * @param string $erro
     * @return void
     */
    private function setErro($erro)
    {
        $this->erro = $erro;
    }

    /**
     * GET erro durante acesso à API do eBank
     * @return string
     */
    public function getErro()
    {
        return $this->erro;
    }

    /**
     * SET propriedade que identifica se está em ambiente de testes (true) ou produção (false)
     * @paramer bool $testando
     * @return void
     */
    public function setTestando($testando)
    {
        $this->testando = $testando;
    }

    /**
     * GET propriedade que identifica se está em ambiente de testes (true) ou produção (false)
     * @return bool
     */
    public function getTestando() {
        return $this->testando;
    }

    /**
     * GET o statusCode da última requisição enviada à API do eBank
     * @return string
     */
    public function getStatusCode()
    {
        return $this->statusCode;
    }

} 
