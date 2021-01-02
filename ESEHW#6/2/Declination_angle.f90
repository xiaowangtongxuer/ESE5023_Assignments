module Declination_angle
implicit none
real(4)  :: N
real(4)  :: angle
contains

subroutine cal_angle(N,angle)
implicit none
real(4)  :: N
real(4)  :: angle
angle = 23.45*sin((N+284)/365*360*(3.14159/180))
end subroutine cal_angle

end module Declination_angle



