#!/usr/bin/Rscript
library(xlsx)

# setwd("../Aplikasi/Sounding/")
# ("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=2019&MONTH=09&FROM=0800&TO=0800&STNM=97180")
rm(list = ls())
load("nama_parameter")

yyyy = "2018"
dayyear = seq(as.Date(paste0(yyyy,"-01", "-01")), as.Date(paste0(yyyy,"-12", "-31")), by = "day")

mm = "08"
i_dat = which(substr(dayyear, 6,7) == mm)
stasiun = "97180"
dd = substr(dayyear, 9,10)
dd = dd[i_dat]
hh = c("00","12")

# hh = rep(hh, length(dd))
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

is.numeric0 <- function(x)
{
  is.numeric(x) && length(x) == 0L
}

instalayer =  function(stasiun, yyyy, mm,  dd, hh ){
  # stasiun = "97180"
  # yyyy="2018"
  #   mm="09"
  #     dd = "17"
  #       hh = "00"
  download.file(paste0("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=",
                       yyyy,"&MONTH=",mm,"&FROM=",dd,hh,"&TO=",dd,hh,"&STNM=",stasiun), 
                mode = "wb", destfile = paste0("input/", stasiun, yyyy, mm, dd, hh, ".txt"))
  txt = read.delim(file = paste0("input/", stasiun, yyyy, mm, dd, hh, ".txt"))
  txt = as.matrix(txt)
  namakolom = strsplit(txt[7], split = " ")[[1]]
  namakolom = namakolom[which(namakolom != "")]
  satuan = strsplit(txt[8], split = " ")[[1]]
  satuan = satuan[which(satuan != "")]
  uniq = "</PRE><H3>Station information and sounding indices</H3><PRE>"
  # 9:(which(txt == uniq)-1)
  dddoo = which(txt == uniq)
  if(is.integer0(dddoo)){
    isi_mat = "No_Data"
    file <- paste0("output/layer/",  stasiun,"_", yyyy, mm, dd,hh, ".xlsx")
    res <- write.xlsx(isi_mat, file, sheetName = "layer") 
    instability = "No_Data"
  }else{
    isidata = txt[10:(dddoo-1)]
    tanda = c(7,14, 21, 28, 35, 42,49, 56, 63, 70, 77)
    isi_mat = matrix(0,ncol = length(tanda), nrow = length(isidata) )
    for(i in 1:length(tanda)){
      for(j in 1:length(isidata)){
        smt = substr(isidata[j], 1, tanda[i])
        smt = strsplit(smt, split = " ")[[1]]
        smt = smt[length(smt)]
        isi_mat[j,i] = as.numeric(smt)
      }
    }
    
    isi_mat = data.frame(as.matrix(isi_mat))
    colnames(isi_mat) = paste0(namakolom, "(",satuan, ")")
    rownames(isi_mat) = NULL
    
    file <- paste0("output/layer/",  stasiun,"_", yyyy, mm, dd,hh, ".xlsx")
    res <- write.xlsx(isi_mat, file, sheetName = "layer") 
    
    
    start_data_index = "</PRE><H3>Station information and sounding indices</H3><PRE>"
    finish_data_index = "<P>Description of the"
    start_data_index = grep(txt, pattern = start_data_index)+1
    finish_data_index = grep(txt, pattern = finish_data_index)-2
    calon_instability = txt[start_data_index:finish_data_index]
    instability = matrix(0, ncol = 2, nrow = length(calon_instability))
    for(i in 1:length(calon_instability)){
      instability[i,] = strsplit(calon_instability[i], split = ": ")[[1]]
    }
    
    instability = data.frame(instability)
    
    colnames(instability) = c("Index", "Value")
    data_hasil_instability = rep("KOSONG", length(nama_parameter))
    man_ins = which( nama_parameter %in% instability$Index)
    data_hasil_instability[man_ins] = as.character(as.matrix(instability[,2]))
    data_hasil_instability[data_hasil_instability == "KOSONG"] = NA
    instability = data.frame(Index = nama_parameter, Value = data_hasil_instability)
    rownames(instability) = NULL
  }
  
  
  
  # tanda = nchar("   19.0  26753  -51.8                          95     19  686.8         686.8")
  # nchar(" 1010.0     14   23.4   20.0     81  14.80      0      0  295.7  338.4  298.3")
  
  
  
  return(list(instability = instability, layer = isi_mat))
}


smt1 = as.character(as.matrix(instalayer(stasiun , yyyy, mm, dd[1], hh[1])$instability[,2]))
smt2 = as.character(as.matrix(instalayer(stasiun , yyyy, mm, dd[1], hh[2])$instability[,2]))
hasil = cbind(smt1, smt2)
nama_para1 = length(instalayer(stasiun , yyyy, mm, dd[1], hh[1])$instability[,1])
nama_para2 = length(instalayer(stasiun , yyyy, mm, dd[1], hh[2])$instability[,1])
nama_para = cbind(nama_para1, nama_para2)

for(i in 1:(length(dd)-1)){
    smt1 = as.character(as.matrix(instalayer(stasiun , yyyy, mm, dd[i+1], hh[1])$instability[,2]))
    smt2 = as.character(as.matrix(instalayer(stasiun , yyyy, mm, dd[i+1], hh[2])$instability[,2]))
    
    hasil = cbind(hasil, smt1, smt2)
    nama_para1 = length(instalayer(stasiun , yyyy, mm, dd[i+1], hh[1])$instability[,1])
    nama_para2 = length(instalayer(stasiun , yyyy, mm, dd[i+1], hh[2])$instability[,1])
    nama_para = c(nama_para,nama_para1, nama_para2)
}

nama_kolom_jam = rep(hh, length(dd))
nama_kolom_hari = list()
for(i in 1:length(dd)){
  nama_kolom_hari[[i]] = rep(dd[i], 2)
}
nama_kolom_hari = c(unlist(nama_kolom_hari))
hasil = data.frame(hasil)
hasilt = t(hasil)
hasil[1:2,2]
hasilt[2,1:2]

colnames(hasilt) = nama_parameter
haritanggal = data.frame(Tanggal = dayyear[i_dat],Jam = nama_kolom_jam)

# load("nama_row_stability") 
hasilx = cbind(haritanggal, hasilt)
rownames(hasilx) = NULL


# which(nama_para == 2)
file <- paste0("output/indeks_stabilitas/",  stasiun,"_", yyyy, mm, ".xlsx")
res <- write.xlsx(data.frame(hasilx), file, sheetName = "Instability")

# nama_parameter = 
instalayer(stasiun , yyyy, mm, dd[1], hh[1])$instability[,2]


