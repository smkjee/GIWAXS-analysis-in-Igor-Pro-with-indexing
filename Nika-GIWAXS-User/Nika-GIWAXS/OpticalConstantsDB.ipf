#pragma rtGlobals=1	// Use modern global access method.
function loadOC()
	string foldersave = getdatafolder(1)
	setdatafolder root:
	newdatafolder /s/o opticalconstants
	newpath /m="Select location of optical constant database: opticalconstants.txt " /O /Q opticalconstants
	LoadWave/J/D/W/O/Q/K=0/A /p=opticalconstants "opticalconstants.txt"
	setdatafolder foldersave
end	
function getdelta(materialname,wavelength,[pathname,aligned])
	string materialname,pathname
	variable wavelength,aligned // aligned is
	if(paramisdefault(pathname))
		pathname = "root:OpticalConstants"
	endif
	
	string foldersave = getdatafolder(1)
	setdatafolder pathname
	aligned = paramisdefault(aligned) ? -1 : aligned
	if(aligned == 0)
		wave deltaw = $("delta_"+materialname +"perp")
		wave enw = $("energy_"+materialname +"perp")
	elseif(aligned == 1)
		wave deltaw = $("delta_"+materialname +"para")
		wave enw = $("energy_"+materialname +"para")
	else
		wave deltaw = $("delta_"+materialname )
		wave enw = $("energy_"+materialname )
	endif
	setdatafolder foldersave
	return interp(1242/wavelength,enw,deltaw)
end
function getbeta(materialname,wavelength,[pathname,aligned])
	string materialname,pathname
	variable wavelength,aligned // aligned is
	if(paramisdefault(pathname))
		pathname = "root:OpticalConstants"
	endif
	
	string foldersave = getdatafolder(1)
	setdatafolder pathname
	aligned = paramisdefault(aligned) ? -1 : aligned
	if(aligned == 0)
		wave betaw = $("beta_"+materialname +"perp")
		wave enw = $("energy_"+materialname +"perp")
	elseif(aligned == 1)
		wave betaw = $("beta_"+materialname +"para")
		wave enw = $("energy_"+materialname +"para")
	else
		wave betaw = $("beta_"+materialname )
		wave enw = $("energy_"+materialname )
	endif
	setdatafolder foldersave
	return interp(1242/wavelength,enw,betaw)
end

function contrast(mat1,mat2,en)
	string mat1, mat2
	variable en
	variable d1,b1,d2,b2
	string foldersave = getdatafolder(1)
	setdatafolder root:opticalconstants
	wave beta1 = $("beta_"+ mat1 )
	wave delta1 = $("delta_"+ mat1 )
	wave en1 = $("energy_"+ mat1 )
	wave beta2 = $("beta_"+ mat2 )
	wave delta2 = $("delta_"+ mat2 )
	wave en2 = $("energy_"+ mat2 )
	d1 = interp(en,en1,delta1)
	b1 = interp(en,en1,beta1)
	d2 = interp(en,en2,delta2)
	b2 = interp(en,en2,beta2)
	setdatafolder foldersave
	return (d1-d2)^2 + (b1-b2)^2
end
function LoadDeltaBetafromFile()
//something
end
function OCsexist(material)
	string material
	string foldersave = getdatafolder(1)
	setdatafolder root:opticalconstants
	variable returnv
	wave/z betapa = $("beta_"+material +"perp")
	wave/z betape = $("beta_"+material +"para")
	wave/z enw = $("energy_"+material )
	wave/z betaw = $("beta_"+material )
	wave/z deltaw = $("delta_"+material )
	if(waveexists(enw) && waveexists(deltaw) && waveexists(betaw))
		if(waveexists(betapa)&&waveexists(betape))
			//aligned optical constants probably exists (at least the betas do)
			returnv = 2
		else
			returnv =  1//basic optical constants exists
		endif
	else
		returnv= 0
	endif
	setdatafolder foldersave
	return returnv
end
function /s listOCs()
	string foldersave = getdatafolder(1)
	setdatafolder root:opticalconstants
	string listofwavesd = wavelist("delta_*",";","")
	string listofwavesb = wavelist("beta_*",";","")
	string listofwavese = wavelist("energy_*",";","")
	setdatafolder foldersave
	variable i
	string materiald, material,listofOCs=""
	for(i=0;i<itemsinlist(listofwavesd);i+=1)
		materiald = stringfromlist(i,listofwavesd)
		material = replacestring("delta_",materiald,"")
		if(findlistitem("beta_"+material, listofwavesb)>=0 &&findlistitem("energy_"+material,listofwavese)>=0 && !stringmatch(material,"*para")&& !stringmatch(material,"*perp") )
			listofOCs = listofOCs + material+";"
		endif
	endfor
	return listofOCs
end