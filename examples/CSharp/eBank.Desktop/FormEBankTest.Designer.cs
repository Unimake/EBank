using System.Drawing;
using System.Windows.Forms;

namespace EBank.Desktop
{
    partial class Form1
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            BtnConsultarBoleto = new Button();
            BtnRegistrarBoleto = new Button();
            TxtResposta = new TextBox();
            BtnExtrato = new Button();
            BtnListarPagamento = new Button();
            BtnAutorizarPagamento = new Button();
            BtnCancelarBoleto = new Button();
            BtnAlterarVencimentoBoleto = new Button();
            BtnInformarPagamentoBoleto = new Button();
            BtnVarredura = new Button();
            BtnConsultaPIX = new Button();
            button1 = new Button();
            tabControl1 = new TabControl();
            tabPage1 = new TabPage();
            tabPage2 = new TabPage();
            tabPage3 = new TabPage();
            tabPage4 = new TabPage();
            BtnSinalizarPIXVencto = new Button();
            BtnVerificarPIXFoiRecebido = new Button();
            ImageQrCodePIX = new PictureBox();
            BtnSinalizarPIX = new Button();
            tabPage5 = new TabPage();
            BtnMsgWhatsBoleto = new Button();
            BtnMsgWhatsPIX = new Button();
            tabControl1.SuspendLayout();
            tabPage1.SuspendLayout();
            tabPage2.SuspendLayout();
            tabPage3.SuspendLayout();
            tabPage4.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)ImageQrCodePIX).BeginInit();
            tabPage5.SuspendLayout();
            SuspendLayout();
            // 
            // BtnConsultarBoleto
            // 
            BtnConsultarBoleto.FlatStyle = FlatStyle.Flat;
            BtnConsultarBoleto.Location = new Point(8, 6);
            BtnConsultarBoleto.Name = "BtnConsultarBoleto";
            BtnConsultarBoleto.Size = new Size(223, 38);
            BtnConsultarBoleto.TabIndex = 0;
            BtnConsultarBoleto.Text = "Consultar boleto";
            BtnConsultarBoleto.UseVisualStyleBackColor = true;
            BtnConsultarBoleto.Click += BtnConsultarBoleto_Click;
            // 
            // BtnRegistrarBoleto
            // 
            BtnRegistrarBoleto.FlatStyle = FlatStyle.Flat;
            BtnRegistrarBoleto.Location = new Point(8, 50);
            BtnRegistrarBoleto.Name = "BtnRegistrarBoleto";
            BtnRegistrarBoleto.Size = new Size(223, 38);
            BtnRegistrarBoleto.TabIndex = 1;
            BtnRegistrarBoleto.Text = "Registrar de boleto";
            BtnRegistrarBoleto.UseVisualStyleBackColor = true;
            BtnRegistrarBoleto.Click += BtnRegistrarBoleto_Click;
            // 
            // TxtResposta
            // 
            TxtResposta.Anchor = AnchorStyles.Top | AnchorStyles.Bottom | AnchorStyles.Left | AnchorStyles.Right;
            TxtResposta.BorderStyle = BorderStyle.FixedSingle;
            TxtResposta.Location = new Point(262, 12);
            TxtResposta.Multiline = true;
            TxtResposta.Name = "TxtResposta";
            TxtResposta.ReadOnly = true;
            TxtResposta.ScrollBars = ScrollBars.Vertical;
            TxtResposta.Size = new Size(1092, 749);
            TxtResposta.TabIndex = 2;
            // 
            // BtnExtrato
            // 
            BtnExtrato.FlatStyle = FlatStyle.Flat;
            BtnExtrato.Location = new Point(8, 6);
            BtnExtrato.Name = "BtnExtrato";
            BtnExtrato.Size = new Size(223, 38);
            BtnExtrato.TabIndex = 3;
            BtnExtrato.Text = "Consulta extrato bancário";
            BtnExtrato.UseVisualStyleBackColor = true;
            BtnExtrato.Click += BtnExtrato_Click;
            // 
            // BtnListarPagamento
            // 
            BtnListarPagamento.FlatStyle = FlatStyle.Flat;
            BtnListarPagamento.Location = new Point(8, 94);
            BtnListarPagamento.Name = "BtnListarPagamento";
            BtnListarPagamento.Size = new Size(223, 38);
            BtnListarPagamento.TabIndex = 4;
            BtnListarPagamento.Text = "Listar pagamentos efetuados";
            BtnListarPagamento.UseVisualStyleBackColor = true;
            BtnListarPagamento.Click += BtnListarPagamento_Click;
            // 
            // BtnAutorizarPagamento
            // 
            BtnAutorizarPagamento.FlatStyle = FlatStyle.Flat;
            BtnAutorizarPagamento.Location = new Point(8, 50);
            BtnAutorizarPagamento.Name = "BtnAutorizarPagamento";
            BtnAutorizarPagamento.Size = new Size(223, 38);
            BtnAutorizarPagamento.TabIndex = 5;
            BtnAutorizarPagamento.Text = "Autorizar Pagamentos";
            BtnAutorizarPagamento.UseVisualStyleBackColor = true;
            BtnAutorizarPagamento.Click += BtnAutorizarPagamento_Click;
            // 
            // BtnCancelarBoleto
            // 
            BtnCancelarBoleto.FlatStyle = FlatStyle.Flat;
            BtnCancelarBoleto.Location = new Point(8, 94);
            BtnCancelarBoleto.Name = "BtnCancelarBoleto";
            BtnCancelarBoleto.Size = new Size(223, 38);
            BtnCancelarBoleto.TabIndex = 6;
            BtnCancelarBoleto.Text = "Cancelar boleto";
            BtnCancelarBoleto.UseVisualStyleBackColor = true;
            BtnCancelarBoleto.Click += BtnCancelarBoleto_Click;
            // 
            // BtnAlterarVencimentoBoleto
            // 
            BtnAlterarVencimentoBoleto.FlatStyle = FlatStyle.Flat;
            BtnAlterarVencimentoBoleto.Location = new Point(8, 138);
            BtnAlterarVencimentoBoleto.Name = "BtnAlterarVencimentoBoleto";
            BtnAlterarVencimentoBoleto.Size = new Size(223, 38);
            BtnAlterarVencimentoBoleto.TabIndex = 7;
            BtnAlterarVencimentoBoleto.Text = "Alterar vencimento boleto";
            BtnAlterarVencimentoBoleto.UseVisualStyleBackColor = true;
            BtnAlterarVencimentoBoleto.Click += BtnAlterarVencimentoBoleto_ClickAsync;
            // 
            // BtnInformarPagamentoBoleto
            // 
            BtnInformarPagamentoBoleto.FlatStyle = FlatStyle.Flat;
            BtnInformarPagamentoBoleto.Location = new Point(8, 182);
            BtnInformarPagamentoBoleto.Name = "BtnInformarPagamentoBoleto";
            BtnInformarPagamentoBoleto.Size = new Size(223, 38);
            BtnInformarPagamentoBoleto.TabIndex = 8;
            BtnInformarPagamentoBoleto.Text = "Informar pagamento boleto";
            BtnInformarPagamentoBoleto.UseVisualStyleBackColor = true;
            BtnInformarPagamentoBoleto.Click += BtnInformarPagamentoBoleto_Click;
            // 
            // BtnVarredura
            // 
            BtnVarredura.FlatStyle = FlatStyle.Flat;
            BtnVarredura.Location = new Point(8, 6);
            BtnVarredura.Name = "BtnVarredura";
            BtnVarredura.Size = new Size(223, 38);
            BtnVarredura.TabIndex = 9;
            BtnVarredura.Text = "Varredura - Consulta boletos a pagar";
            BtnVarredura.UseVisualStyleBackColor = true;
            BtnVarredura.Click += BtnVarredura_Click;
            // 
            // BtnConsultaPIX
            // 
            BtnConsultaPIX.FlatStyle = FlatStyle.Flat;
            BtnConsultaPIX.Location = new Point(8, 6);
            BtnConsultaPIX.Name = "BtnConsultaPIX";
            BtnConsultaPIX.Size = new Size(223, 38);
            BtnConsultaPIX.TabIndex = 10;
            BtnConsultaPIX.Text = "Consulta PIX por período";
            BtnConsultaPIX.UseVisualStyleBackColor = true;
            BtnConsultaPIX.Click += BtnConsultaPIX_Click;
            // 
            // button1
            // 
            button1.FlatStyle = FlatStyle.Flat;
            button1.Location = new Point(8, 50);
            button1.Name = "button1";
            button1.Size = new Size(223, 38);
            button1.TabIndex = 11;
            button1.Text = "Consulta PIX por TxId";
            button1.UseVisualStyleBackColor = true;
            button1.Click += Button1_Click;
            // 
            // tabControl1
            // 
            tabControl1.Anchor = AnchorStyles.Top | AnchorStyles.Bottom | AnchorStyles.Left;
            tabControl1.Controls.Add(tabPage1);
            tabControl1.Controls.Add(tabPage2);
            tabControl1.Controls.Add(tabPage3);
            tabControl1.Controls.Add(tabPage4);
            tabControl1.Controls.Add(tabPage5);
            tabControl1.Location = new Point(12, 12);
            tabControl1.Name = "tabControl1";
            tabControl1.SelectedIndex = 0;
            tabControl1.Size = new Size(248, 749);
            tabControl1.TabIndex = 12;
            // 
            // tabPage1
            // 
            tabPage1.Controls.Add(BtnConsultarBoleto);
            tabPage1.Controls.Add(BtnRegistrarBoleto);
            tabPage1.Controls.Add(BtnInformarPagamentoBoleto);
            tabPage1.Controls.Add(BtnAlterarVencimentoBoleto);
            tabPage1.Controls.Add(BtnCancelarBoleto);
            tabPage1.Location = new Point(4, 24);
            tabPage1.Name = "tabPage1";
            tabPage1.Padding = new Padding(3, 3, 3, 3);
            tabPage1.Size = new Size(240, 721);
            tabPage1.TabIndex = 0;
            tabPage1.Text = "Boletos";
            tabPage1.UseVisualStyleBackColor = true;
            // 
            // tabPage2
            // 
            tabPage2.Controls.Add(BtnExtrato);
            tabPage2.Location = new Point(4, 24);
            tabPage2.Name = "tabPage2";
            tabPage2.Padding = new Padding(3, 3, 3, 3);
            tabPage2.Size = new Size(240, 721);
            tabPage2.TabIndex = 1;
            tabPage2.Text = "Extrato";
            tabPage2.UseVisualStyleBackColor = true;
            // 
            // tabPage3
            // 
            tabPage3.Controls.Add(BtnVarredura);
            tabPage3.Controls.Add(BtnAutorizarPagamento);
            tabPage3.Controls.Add(BtnListarPagamento);
            tabPage3.Location = new Point(4, 24);
            tabPage3.Name = "tabPage3";
            tabPage3.Padding = new Padding(3, 3, 3, 3);
            tabPage3.Size = new Size(240, 721);
            tabPage3.TabIndex = 2;
            tabPage3.Text = "Pagamentos";
            tabPage3.UseVisualStyleBackColor = true;
            // 
            // tabPage4
            // 
            tabPage4.Controls.Add(BtnSinalizarPIXVencto);
            tabPage4.Controls.Add(BtnVerificarPIXFoiRecebido);
            tabPage4.Controls.Add(ImageQrCodePIX);
            tabPage4.Controls.Add(BtnSinalizarPIX);
            tabPage4.Controls.Add(BtnConsultaPIX);
            tabPage4.Controls.Add(button1);
            tabPage4.Location = new Point(4, 24);
            tabPage4.Name = "tabPage4";
            tabPage4.Padding = new Padding(3, 3, 3, 3);
            tabPage4.Size = new Size(240, 721);
            tabPage4.TabIndex = 3;
            tabPage4.Text = "PIX";
            tabPage4.UseVisualStyleBackColor = true;
            // 
            // BtnSinalizarPIXVencto
            // 
            BtnSinalizarPIXVencto.FlatStyle = FlatStyle.Flat;
            BtnSinalizarPIXVencto.Location = new Point(8, 472);
            BtnSinalizarPIXVencto.Name = "BtnSinalizarPIXVencto";
            BtnSinalizarPIXVencto.Size = new Size(223, 63);
            BtnSinalizarPIXVencto.TabIndex = 15;
            BtnSinalizarPIXVencto.Text = "Sinalizar para o banco um recebimento via PIX com vencimento e obter o QRCode";
            BtnSinalizarPIXVencto.UseVisualStyleBackColor = true;
            BtnSinalizarPIXVencto.Click += BtnSinalizarPIXVencto_Click;
            // 
            // BtnVerificarPIXFoiRecebido
            // 
            BtnVerificarPIXFoiRecebido.FlatStyle = FlatStyle.Flat;
            BtnVerificarPIXFoiRecebido.Location = new Point(8, 399);
            BtnVerificarPIXFoiRecebido.Name = "BtnVerificarPIXFoiRecebido";
            BtnVerificarPIXFoiRecebido.Size = new Size(223, 49);
            BtnVerificarPIXFoiRecebido.TabIndex = 14;
            BtnVerificarPIXFoiRecebido.Text = "Verificar se o PIX foi efetuado pelo pagador";
            BtnVerificarPIXFoiRecebido.UseVisualStyleBackColor = true;
            BtnVerificarPIXFoiRecebido.Click += BtnVerificarPIXFoiRecebido_Click;
            // 
            // ImageQrCodePIX
            // 
            ImageQrCodePIX.Location = new Point(10, 172);
            ImageQrCodePIX.Name = "ImageQrCodePIX";
            ImageQrCodePIX.Size = new Size(221, 221);
            ImageQrCodePIX.TabIndex = 13;
            ImageQrCodePIX.TabStop = false;
            // 
            // BtnSinalizarPIX
            // 
            BtnSinalizarPIX.FlatStyle = FlatStyle.Flat;
            BtnSinalizarPIX.Location = new Point(8, 112);
            BtnSinalizarPIX.Name = "BtnSinalizarPIX";
            BtnSinalizarPIX.Size = new Size(223, 49);
            BtnSinalizarPIX.TabIndex = 12;
            BtnSinalizarPIX.Text = "Sinalizar para o banco um recebimento via PIX e obter o QRCode";
            BtnSinalizarPIX.UseVisualStyleBackColor = true;
            BtnSinalizarPIX.Click += BtnSinalizarPIX_Click;
            // 
            // tabPage5
            // 
            tabPage5.Controls.Add(BtnMsgWhatsBoleto);
            tabPage5.Controls.Add(BtnMsgWhatsPIX);
            tabPage5.Location = new Point(4, 24);
            tabPage5.Margin = new Padding(3, 2, 3, 2);
            tabPage5.Name = "tabPage5";
            tabPage5.Padding = new Padding(3, 2, 3, 2);
            tabPage5.Size = new Size(240, 721);
            tabPage5.TabIndex = 4;
            tabPage5.Text = "WhatsApp";
            tabPage5.UseVisualStyleBackColor = true;
            // 
            // BtnMsgWhatsBoleto
            // 
            BtnMsgWhatsBoleto.FlatStyle = FlatStyle.Flat;
            BtnMsgWhatsBoleto.Location = new Point(9, 50);
            BtnMsgWhatsBoleto.Name = "BtnMsgWhatsBoleto";
            BtnMsgWhatsBoleto.Size = new Size(223, 38);
            BtnMsgWhatsBoleto.TabIndex = 12;
            BtnMsgWhatsBoleto.Text = "Mensagem Whats Boleto";
            BtnMsgWhatsBoleto.UseVisualStyleBackColor = true;
            BtnMsgWhatsBoleto.Click += BtnMsgWhatsBoleto_Click;
            // 
            // BtnMsgWhatsPIX
            // 
            BtnMsgWhatsPIX.FlatStyle = FlatStyle.Flat;
            BtnMsgWhatsPIX.Location = new Point(9, 5);
            BtnMsgWhatsPIX.Name = "BtnMsgWhatsPIX";
            BtnMsgWhatsPIX.Size = new Size(223, 38);
            BtnMsgWhatsPIX.TabIndex = 11;
            BtnMsgWhatsPIX.Text = "Mensagem Whats PIX";
            BtnMsgWhatsPIX.UseVisualStyleBackColor = true;
            BtnMsgWhatsPIX.Click += BtnMsgWhatsPIX_Click;
            // 
            // Form1
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(1366, 773);
            Controls.Add(tabControl1);
            Controls.Add(TxtResposta);
            Name = "Form1";
            Text = "EBank - Unimake";
            tabControl1.ResumeLayout(false);
            tabPage1.ResumeLayout(false);
            tabPage2.ResumeLayout(false);
            tabPage3.ResumeLayout(false);
            tabPage4.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)ImageQrCodePIX).EndInit();
            tabPage5.ResumeLayout(false);
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private Button BtnConsultarBoleto;
        private Button BtnRegistrarBoleto;
        private TextBox TxtResposta;
        private Button BtnExtrato;
        private Button BtnListarPagamento;
        private Button BtnAutorizarPagamento;
        private Button BtnCancelarBoleto;
        private Button BtnAlterarVencimentoBoleto;
        private Button BtnInformarPagamentoBoleto;
        private Button BtnVarredura;
        private Button BtnConsultaPIX;
        private Button button1;
        private TabControl tabControl1;
        private TabPage tabPage1;
        private TabPage tabPage2;
        private TabPage tabPage3;
        private TabPage tabPage4;
        private Button BtnSinalizarPIX;
        private PictureBox ImageQrCodePIX;
        private Button BtnVerificarPIXFoiRecebido;
        private Button BtnSinalizarPIXVencto;
        private TabPage tabPage5;
        private Button BtnMsgWhatsPIX;
        private Button BtnMsgWhatsBoleto;
    }
}