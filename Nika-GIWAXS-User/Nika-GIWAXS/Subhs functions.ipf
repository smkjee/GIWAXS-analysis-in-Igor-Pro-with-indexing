function patchuppolefig(starti, endi)
//patch one side of pole figure by taking a slice of data from the opposute side
//set data folder where the original pole figure wave is located. 
//the pole figure wave must be named SinCorrPole
//starti and endi - start and end angles of the bad portion that needs to be replaced
variable starti, endi
//setdatafolder root:Packages:Convert2Dto1D:PoleFig:
if (starti < endi)
	if (abs(starti) <= 90)
		if (abs(endi) <= 90)
			wave /Z SinCorrPole, SinCorrPole_patched
			Redimension/N=181 SinCorrPole
			duplicate /O SinCorrPole SinCorrPole_patched
				duplicate /O SinCorrPole SinCorrPole_patched
				variable i
				i=starti
				do
					SinCorrPole_patched[90+i]=SinCorrPole[90-i]
					i=i+1
				while  (i <= endi)
//				appendtoGraph /R/W=Convert2Dto1DPoleFig SinCorrPole_patched
		else
			Print "ERROR: end angle magnitide should be less than 90"
		endif
	else
		Print "ERROR: end angle magnitide should be less than 90"
	endif
else
	Print "ERROR: start angle should be less than end angle"
endif
//replace negatives by zeros
MatrixOP/O SinCorrPole_patched = SinCorrPole_patched * replace(sgn(SinCorrPole_patched), -1, 0)
//integrate pole figure
//integratepole(SinCorrPole_patched)
end

function integratepole(polewave,starta,enda)
	wave /z polewave
	variable starta, enda
	MatrixOP/O polewave = polewave*replace(sgn(polewave), -1, 0)
	duplicate /O polewave polewave_temp
	wave /z polewave_temp
	MatrixOP/O polewave_temp = replaceNaNs(polewave_temp,0)
	variable /g IntPole
	IntPole = area(polewave_temp, starta, enda)
	killwaves /z polewave_temp
	print IntPole
end

function orientparam(sincorrpolefig, minX, maxX)
	wave sincorrpolefig
	variable MinX, maxX
	wave /z polewave_temp
	
	MatrixOP/O polewave_temp = sincorrpolefig*replace(sgn(sincorrpolefig), -1, 0)
	MatrixOP/O polewave_temp = replaceNaNs(polewave_temp,0)
	
	duplicate/O/R=(MinX,maxX) SinCorrPoleFig Normalisation  //Subh add
	duplicate/O/R=(MinX,maxX) SinCorrPoleFig Zaehler        //Subh add
	duplicate/O/R=(MinX,maxX) SinCorrPoleFig AngleInRad     //Subh add
	
	Variable Offset = dimOffset(SinCorrPoleFig,0)
	Variable delta = dimDelta(SinCorrPoleFig,0)
	AngleInRad = x/180*pi	
		
	Zaehler = Normalisation * (cos(AngleInRad))^2  //Subh add
	
	//The Integration is from a small angle to pi/half to avoid the evanescent field near the horizon
	//Its hard to estimate the error due to the missing wedge in the data 
	variable Orientation = 0.5*(3*AreaXY(AngleInRad,Zaehler,MinX/180*pi,MaxX/180*pi)/AreaXY(AngleInRad,Normalisation,MinX*180/pi,MaxX/180*pi)-1)    //Subh add
	print "foldername = ", getdatafolder(1)
	print "Orientation = ", Orientation  //Subh add
	
	Killwaves/Z Normalisation, Zaehler, AngleInRad, polewave_temp
	return Orientation
end