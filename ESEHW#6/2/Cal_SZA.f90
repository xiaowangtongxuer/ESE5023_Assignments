program Cal_SZA
use Declination_angle
use cal_AST

implicit none

real(4) :: N1,LSTM1,H1,SZA,cosSZA
real(4) :: lon1,lat1,lst1,ast1,angle1,et1

write(*,*) 'please input N, such as 202, instead of 2012-7-21'
read(*,*) N1
write(*,*) 'please input lon, such as 112.00, instead of 112.00°'
read(*,*) lon1
write(*,*) 'please input lat, such as 33.43, instead of 33.43°'
read(*,*) lat1
write(*,*) 'please input LST, such as 8.50, instead of 8:30'
read(*,*) lst1

call cal_angle(N1,angle1)
call calcu_ast(lst1,N1,lon1,ast1)

H1 = (ast1*60-720)/4
cosSZA = cos(lat1*3.14159/180)*cos(angle1*3.14159/180)*cos(H1*3.14159/180)+sin(lat1*3.14159/180)*sin(angle1*3.14159/180)
SZA = ACOS(cosSZA)/3.14159*180
!write(*,*) 'angle=',angle1
!write(*,*) 'AST=',ast1
write(*,*) 'So,the calculated SZA = ',SZA

end program Cal_SZA



