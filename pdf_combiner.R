
  require(pdftools)

  pdf_list = c()
  pdf_list=append(pdf_list,"1.pdf")
  pdf_list=append(pdf_list,"2.pdf")
      
  pdf_combine(pdf_list,output="combined.pdf")
