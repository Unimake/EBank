<?php

require_once 'eBank.class.php';

class teste
{
    private $referencia;
    private $numeroBoleto;
    private $vencimento;
    private $base = [];
    private $eBank;

    public function __construct()
    {
        $this->eBank = new eBankClass();
        $this->eBank->setTestando(true); // true = ambiente de testes, false = ambiente de produção
    }

    public function testaEbank()
    {
        // gera informacoes aleatórias para o boleto
        $this->referencia = strval(rand(100000, 999999)); // Referência do boleto como string. Exemplo: número do pedido, nota fiscal, etc. Tipo STRING.
        $this->numeroBoleto = strval(rand(100000000, 999999999)); // Número do boleto no banco. Tipo STRING.
        $this->vencimento = date("Y-m-d", strtotime("+7 days")); // Data de vencimento do boleto (AAAA-MM-DD)

        echo "Iniciando teste com o eBank...<br>";
        echo "- Referência: " . $this->referencia . "<br>";
        echo "- Número do boleto: " . $this->numeroBoleto . "<br>";
        echo "- Data de vencimento: " . $this->vencimento . "<br>";

        // Registra um boleto
        if($this->registraBoleto()) {
            echo "- Boleto registrado com sucesso!<br>";
            echo "- Linha digitável: " . $this->eBank->getResponse("LinhaDigitavel"). "<br>";

            // salva o PDF do boleto
            if($this->salvaPDF('boleto.pdf')) {
                echo "- PDF do boleto gerado com sucesso!<br>";
            } else {
                echo "- não foi possível gerar o PDF<br>";
            }

            // altera o vencimento do boleto
            $this->vencimento = date("Y-m-d", strtotime("+10 days")); // nova data de vencimento
            if($this->alteraVencimentoBoleto($this->numeroBoleto, $this->vencimento)) {
                echo "- Vencimento do boleto alterado para {$this->vencimento} com sucesso!<br>";
            } else {
                echo "- Erro ao alterar vencimento: " . $this->eBank->getErro() . "<br>";
            }

            // concede um desconto de R$ 100,00 ao boleto
            $dataLimiteDesconto = $this->vencimento; // Data de validade do desconto
            if($this->concedeDescontoBoleto($this->numeroBoleto, 100.00, $dataLimiteDesconto)) {
                echo "- Desconto de R$ 100,00 concedido ao boleto!<br>";
            } else {
                echo "- Erro ao conceder desconto: " . $this->eBank->getErro() . "<br>";
            }

            // altera o desconto do boleto para R$ 58,00
            if($this->concedeDescontoBoleto($this->numeroBoleto, 58.00, $dataLimiteDesconto)) {
                echo "- Desconto de R$ 58,00 concedido ao boleto!<br>";
            } else {
                echo "- Erro ao conceder desconto: " . $this->eBank->getErro() . "<br>";
            }

            // exclui o desconto do boleto
            if($this->cancelaDescontoBoleto($this->numeroBoleto)) {
                echo "- Desconto do boleto excluído com sucesso!<br>";
            } else {
                echo "- Erro ao excluir desconto: " . $this->eBank->getErro() . "<br>";
            }

            // cancela o boleto
            /*
            if($this->cancelaBoleto($this->numeroBoleto)) {
                echo "- Boleto cancelado com sucesso!<br>";
            } else {
                echo "- Erro ao cancelar boleto: " . $this->eBank->getErro() . "<br>";
            }
            */

        } else {
            echo "Erro ao registrar boleto: " . $this->eBank->getErro() . "<br>";
        }
    }

    /**
     * Registra um boleto no banco
     * @return bool
     */
    public function registraBoleto()
    {
        $this->base = [];
        $this->configuraBeneficiario();
        $this->configuraBoleto();
        $this->configuraMensagens();
        $this->configuraJuros();
        $this->configuraMulta();
        $this->configuraPagador();
        $this->configuraPDF();
        $this->configuraBoletoHibrido();
        $this->configuraTeste();

        return $this->eBank->registraBoleto( $this->jsonText('registro') );
    }

    public function salvaPDF($nomeArquivo)
    {
        $grupoPDF = $this->eBank->getResponse("PDFContent");
        $conteudoPDF = $grupoPDF["Content"];
        if ($conteudoPDF) {
            $arqPDF = "{$this->numeroBoleto}.pdf";

            // Salva o PDF
            $conteudoPDF = base64_decode($conteudoPDF);
            file_put_contents($arqPDF, $conteudoPDF);
            if(file_exists($arqPDF)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Alterar o vencimento do boleto no banco
     * 
     * @param string $numeroBoleto - número do boleto no banco
     * @param string $vencimento - nova data de vencimento do boleto. Formato AAAAMMDD
     * @return bool
     */
    public function alteraVencimentoBoleto($numeroBoleto, $vencimento)
    {
        $this->base = [];
        $this->configuraBeneficiario();
        $this->base["instrucao"] = '3';  // <-- 3=alteração de vencimento
        $this->base["data"] = $vencimento;
        $this->base["numeroNoBanco"] = $numeroBoleto;
        $this->configuraTeste();

        return $this->eBank->alteraVencimentoBoleto( $this->jsonText('altVenc') );
    }

    /**
     * Conceder ou altera o desconto a um boleto já registrado no banco.
     * 
     * @param string $numeroBoleto - número do boleto no banco 
     * @param numeric $descontos - novo valor do desconto.  Pode ser usado na concessão do desconto bem como na alteração do desconto
     * @param string $dataValidade - data de validade do desconto. Formato AAAAMMDD
     * @return bool
     */
    public function concedeDescontoBoleto($numeroBoleto, $descontos, $dataValidade)
    {
        $this->base = [];
        $this->configuraBeneficiario();
        $this->base["numeroNoBanco"] = $numeroBoleto;
        $this->base["data"] = $dataValidade;
        $this->base["instrucao"] = '1';  // <-- 1=Concessão de desconto
        $this->base["tipoDesconto"] = '1';  // <-- 1=Valor fixo até data informada
        $this->base["valor"] = round($descontos, 2); // formata o desconto com duas casas decimais
        $this->configuraTeste();

        return $this->eBank->concedeDescontoBoleto( $this->jsonText("concDesc$descontos") );
    }

    /**
     * Exclui o desconto de um boleto já registrado no banco
     * 
     * @param string $numeroBoleto - número do boleto no banco
     * @return bool
     */
    public function cancelaDescontoBoleto($numeroBoleto)
    {
        $this->base = [];
        $this->configuraBeneficiario();
        $this->base["numeroNoBanco"] = $numeroBoleto;
        $this->base["instrucao"] = '2';  // <-- 2=Cancelamento de desconto concedido
        $this->configuraTeste();

        return $this->eBank->cancelaDescontoBoleto(  $this->jsonText('cancDesc') );
    }

    /**
     * Cancelar o boleto no banco
     * 
     * @param string $numeroBoleto - número do boleto no banco. 
     * @return bool
     */
    public function cancelaBoleto($numeroBoleto)
    {
        $this->base = [];
        $this->configuraBeneficiario();
        $this->base["numeroNoBanco"] = $numeroBoleto;
        $this->configuraTeste();

        return $this->eBank->cancelaBoleto( $this->jsonText('cancBoleto') );
    }

    /**
     * Método para gerar o JSON do boleto. 
     *      1. Converte o array em texto
     *      2. Salva o json texto em um arquivo, para depuração
     *      3. Retorna o json texto
     * 
     * @return string
     */
    private function jsonText($metodo) {
        $jsonLog = json_encode($this->base, JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE);
        file_put_contents("boleto-{$this->numeroBoleto}-$metodo.json", $jsonLog); // Salva o JSON em um arquivo para depuração, formatado

        $jsonText = json_encode($this->base, JSON_UNESCAPED_UNICODE); // sem formatação, para enviar ao eBank
        return $jsonText;
    }

    private function configuraBeneficiario()
    {
        $conta = [];
        $conta["agencia"] = "4340";
        $conta["banco"] = "756";
        $conta["digitoVerificador"] = "4"; // DV do banco
        $conta["numero"] = "00001";

        $beneficiario = [];
        $beneficiario["codigo"] = "000014340"; // código do cedente
        $beneficiario["conta"] = $conta; // conta bancária do cedente
        $beneficiario["inscricao"] = "06117473000079"; // CNPJ do cedente, sem formatação
        $beneficiario["nome"] = "Unifake Software"; // Nome do cedente

        $this->base['beneficiario'] = $beneficiario;
    }

    private function configuraBoleto()
    {

        $this->base["especie"] = '2';
        $this->base["numeroNoBanco"] = $this->numeroBoleto;
        $this->base["numeroNaEmpresa"] = $this->referencia; // Referência do boleto como string. Exemplo: número do pedido, nota fiscal, etc. Tipo STRING.
        $this->base["vencimento"] = $this->vencimento;  // AAAA-MM-DD
        $this->base["emissao"] = date("Y-m-d");  // AAAAMMDD
        $this->base["diasParaBaixaOuDevolucao"] = 0;
        $this->base["tipoBaixaDevolucao"] = "1";
        $this->base["carteira"] = '1';
        $this->base["valorIof"] = 0;
        $this->base["valornominal"] = 1234.56;
        $this->base["valorAbatimento"] = 0;
    }

    private function configuraMensagens()
    {
        $mensagens = ["Não receber após vencimento", "Outra mensagem qualquer"];
        $this->base["mensagens"] = $mensagens;
    }

    private function configuraJuros()
    {
        $juros = [];
        $juros['data'] = date("Y-m-d", strtotime("+8 days")); // Data de início dos juros
        $juros['tipo'] = '1';  // 1=ValorPorDia 2=TaxaMensal 3=Isento 4=Percentual 5=PercentualPorMes 6=PercentualPorAno
        $juros['valor'] = 0.89; // Valor do juros por dia de atraso (R$ 0,89)
        $this->base['juros'] = $juros;
    }

    private function configuraMulta()
    {
        $multa = [];
        $multa['data'] = date("Y-m-d", strtotime("+8 days")); // Data de início da multa
        $multa['tipo'] = '1';  // 0=SemMulta multa 1=ValorFixo 2=Percentual 3=PercentualMes
        $multa['valor'] = 62.00; // Valor da multa
        $this->base['multa'] = $multa;
    }

    private function configuraPagador()
    {
        $endereco = [];
        $endereco["rua"] = "RUA DOS FULANOS";
        $endereco["numero"] = "120";
        $endereco["bairro"] = "CENTRO";
        $endereco["cep"] = "12345678"; // CEP sem formatação
        $endereco["cidade"] = "FULANÓPOLIS";
        $endereco["uf"] = "SP";

        $pagador = [];
        $pagador["nome"] = "FULANO BELTRANO DA SILVA";
        $pagador["tipoInscricao"] = "1";  // CNPJ
        $pagador["inscricao"] = "123.456.789-00";
        $pagador["email"] = "fulano.beltrano@gmail.com";
        $pagador["telefone"] = "11987654321"; // Telefone sem formatação
        $pagador["endereco"] = $endereco;

        $this->base["pagador"] = $pagador;
    }

    private function configuraPDF()
    {
        $pdf = [];
        $pdf["password"] = ""; // Se quiser proteger o PDF, defina uma senha aqui. Senão, deixe em branco.
        $pdf["permitAnnotations"] = false;  // Permite anotações no PDF
        $pdf["permitAssembleDocument"] = true; // Permite que o PDF seja montado com outros documentos
        $pdf["permitExtractContent"] = true; // Permite extrair conteúdo do PDF
        $pdf["permitFormsFill"] = false; // Permite preencher formulários no PDF
        $pdf["permitFullQualityPrint"] = true; // Permite impressão em alta qualidade
        $pdf["permitModifyDocument"] = false; // Permite modificar o documento PDF
        $pdf["permitPrint"] = true; // Permite impressão do PDF
        $pdf["signPDF"] = false; // Se quiser assinar digitalmente o PDF, defina como true. Senão, deixe em false.
        $pdf["tryGeneratePDF"] = true; // Tenta gerar o PDF mesmo que não tenha sido registrado no banco
        $this->base["pdfConfig"] = $pdf;
    }

    private function configuraBoletoHibrido()
    {
        $pixConfig = [];
        $pixConfig["chave"] = '06117473000079'; // Chave Pix do beneficiário
        $pixConfig["registrarPIX"] = true; // Registrar o PIX junto com o boleto
        $this->base["pixConfig"] = $pixConfig;
    }

    private function configuraTeste()
    {
        $this->base["testing"] = $this->eBank->getTestando();
    }    


}

$meuTeste = new teste();
$meuTeste->testaEbank();