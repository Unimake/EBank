namespace eBankTest
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
            this.BtnConsultarBoleto = new System.Windows.Forms.Button();
            this.BtnRegistrarBoleto = new System.Windows.Forms.Button();
            this.TxtResposta = new System.Windows.Forms.TextBox();
            this.BtnExtrato = new System.Windows.Forms.Button();
            this.BtnListarPagamento = new System.Windows.Forms.Button();
            this.BtnAutorizarPagamento = new System.Windows.Forms.Button();
            this.BtnCancelarBoleto = new System.Windows.Forms.Button();
            this.BtnAlterarVencimentoBoleto = new System.Windows.Forms.Button();
            this.BtnInformarPagamentoBoleto = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // BtnConsultarBoleto
            // 
            this.BtnConsultarBoleto.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnConsultarBoleto.Location = new System.Drawing.Point(8, 12);
            this.BtnConsultarBoleto.Name = "BtnConsultarBoleto";
            this.BtnConsultarBoleto.Size = new System.Drawing.Size(223, 38);
            this.BtnConsultarBoleto.TabIndex = 0;
            this.BtnConsultarBoleto.Text = "Consultar boleto";
            this.BtnConsultarBoleto.UseVisualStyleBackColor = true;
            this.BtnConsultarBoleto.Click += new System.EventHandler(this.BtnConsultarBoleto_ClickAsync);
            // 
            // BtnRegistrarBoleto
            // 
            this.BtnRegistrarBoleto.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnRegistrarBoleto.Location = new System.Drawing.Point(8, 56);
            this.BtnRegistrarBoleto.Name = "BtnRegistrarBoleto";
            this.BtnRegistrarBoleto.Size = new System.Drawing.Size(223, 38);
            this.BtnRegistrarBoleto.TabIndex = 1;
            this.BtnRegistrarBoleto.Text = "Registrar de boleto";
            this.BtnRegistrarBoleto.UseVisualStyleBackColor = true;
            this.BtnRegistrarBoleto.Click += new System.EventHandler(this.BtnRegistrarBoleto_Click);
            // 
            // TxtResposta
            // 
            this.TxtResposta.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.TxtResposta.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.TxtResposta.Location = new System.Drawing.Point(237, 12);
            this.TxtResposta.Multiline = true;
            this.TxtResposta.Name = "TxtResposta";
            this.TxtResposta.ReadOnly = true;
            this.TxtResposta.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.TxtResposta.Size = new System.Drawing.Size(551, 603);
            this.TxtResposta.TabIndex = 2;
            // 
            // BtnExtrato
            // 
            this.BtnExtrato.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnExtrato.Location = new System.Drawing.Point(8, 283);
            this.BtnExtrato.Name = "BtnExtrato";
            this.BtnExtrato.Size = new System.Drawing.Size(223, 38);
            this.BtnExtrato.TabIndex = 3;
            this.BtnExtrato.Text = "Extrato";
            this.BtnExtrato.UseVisualStyleBackColor = true;
            this.BtnExtrato.Click += new System.EventHandler(this.BtnExtrato_Click);
            // 
            // BtnListarPagamento
            // 
            this.BtnListarPagamento.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnListarPagamento.Location = new System.Drawing.Point(8, 380);
            this.BtnListarPagamento.Name = "BtnListarPagamento";
            this.BtnListarPagamento.Size = new System.Drawing.Size(223, 38);
            this.BtnListarPagamento.TabIndex = 4;
            this.BtnListarPagamento.Text = "Listar pagamentos";
            this.BtnListarPagamento.UseVisualStyleBackColor = true;
            this.BtnListarPagamento.Click += new System.EventHandler(this.BtnListarPagamento_Click);
            // 
            // BtnAutorizarPagamento
            // 
            this.BtnAutorizarPagamento.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnAutorizarPagamento.Location = new System.Drawing.Point(8, 424);
            this.BtnAutorizarPagamento.Name = "BtnAutorizarPagamento";
            this.BtnAutorizarPagamento.Size = new System.Drawing.Size(223, 38);
            this.BtnAutorizarPagamento.TabIndex = 5;
            this.BtnAutorizarPagamento.Text = "Autorizar Pagamentos";
            this.BtnAutorizarPagamento.UseVisualStyleBackColor = true;
            this.BtnAutorizarPagamento.Click += new System.EventHandler(this.BtnAutorizarPagamento_Click);
            // 
            // BtnCancelarBoleto
            // 
            this.BtnCancelarBoleto.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnCancelarBoleto.Location = new System.Drawing.Point(8, 100);
            this.BtnCancelarBoleto.Name = "BtnCancelarBoleto";
            this.BtnCancelarBoleto.Size = new System.Drawing.Size(223, 38);
            this.BtnCancelarBoleto.TabIndex = 6;
            this.BtnCancelarBoleto.Text = "Cancelar boleto";
            this.BtnCancelarBoleto.UseVisualStyleBackColor = true;
            this.BtnCancelarBoleto.Click += new System.EventHandler(this.BtnCancelarBoleto_Click);
            // 
            // BtnAlterarVencimentoBoleto
            // 
            this.BtnAlterarVencimentoBoleto.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnAlterarVencimentoBoleto.Location = new System.Drawing.Point(8, 144);
            this.BtnAlterarVencimentoBoleto.Name = "BtnAlterarVencimentoBoleto";
            this.BtnAlterarVencimentoBoleto.Size = new System.Drawing.Size(223, 38);
            this.BtnAlterarVencimentoBoleto.TabIndex = 7;
            this.BtnAlterarVencimentoBoleto.Text = "Alterar vencimento boleto";
            this.BtnAlterarVencimentoBoleto.UseVisualStyleBackColor = true;
            this.BtnAlterarVencimentoBoleto.Click += new System.EventHandler(this.BtnAlterarVencimentoBoleto_Click);
            // 
            // BtnInformarPagamentoBoleto
            // 
            this.BtnInformarPagamentoBoleto.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnInformarPagamentoBoleto.Location = new System.Drawing.Point(8, 188);
            this.BtnInformarPagamentoBoleto.Name = "BtnInformarPagamentoBoleto";
            this.BtnInformarPagamentoBoleto.Size = new System.Drawing.Size(223, 38);
            this.BtnInformarPagamentoBoleto.TabIndex = 8;
            this.BtnInformarPagamentoBoleto.Text = "Informar pagamento boleto";
            this.BtnInformarPagamentoBoleto.UseVisualStyleBackColor = true;
            this.BtnInformarPagamentoBoleto.Click += new System.EventHandler(this.BtnInformarPagamentoBoleto_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(800, 627);
            this.Controls.Add(this.BtnInformarPagamentoBoleto);
            this.Controls.Add(this.BtnAlterarVencimentoBoleto);
            this.Controls.Add(this.BtnCancelarBoleto);
            this.Controls.Add(this.BtnAutorizarPagamento);
            this.Controls.Add(this.BtnListarPagamento);
            this.Controls.Add(this.BtnExtrato);
            this.Controls.Add(this.TxtResposta);
            this.Controls.Add(this.BtnRegistrarBoleto);
            this.Controls.Add(this.BtnConsultarBoleto);
            this.Name = "Form1";
            this.Text = "eBank - Unimake";
            this.ResumeLayout(false);
            this.PerformLayout();

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
    }
}