VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmMain 
   Caption         =   "Unimake.DFe Interop Tests"
   ClientHeight    =   11850
   ClientLeft      =   60
   ClientTop       =   705
   ClientWidth     =   11580
   LinkTopic       =   "Form1"
   ScaleHeight     =   11850
   ScaleWidth      =   11580
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog OpenFileDialog 
      Left            =   840
      Top             =   10215
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Menu mnuPIX 
      Caption         =   "ePIX"
      Begin VB.Menu mnuPIX_AutenticarAPI 
         Caption         =   "Autenticar API"
      End
      Begin VB.Menu mnuPIX_GerarPIX 
         Caption         =   "Gerar PIX"
      End
      Begin VB.Menu mnuPIX_ConsultarPIX 
         Caption         =   "Consultar PIX"
      End
   End
   Begin VB.Menu mnuBoleto 
      Caption         =   "eBoleto"
      Begin VB.Menu mnuBoleto_RegistrarBoleto 
         Caption         =   "Registrar Boleto"
      End
      Begin VB.Menu mnuBoleto_ConsultarBoleto 
         Caption         =   "Consultar Boleto"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit



Private Sub Form_Load()
Utils.hWnd = Me.hWnd
End Sub

Private Sub mnuPIX_AutenticarAPI_Click()
   AutenticarAPI
End Sub


Private Sub mnuPIX_GerarPIX_Click()
   GerarPIX
End Sub

Private Sub mnuPIX_ConsultarPIX_Click()
   ConsultarPIX
End Sub

Private Sub mnuBoleto_RegistrarBoleto_Click()
   RegistrarBoleto
End Sub

Private Sub mnuBoleto_ConsultarBoleto_Click()
   ConsultarBoleto
End Sub

