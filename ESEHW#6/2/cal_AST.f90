module cal_AST
implicit none
real(4) :: N0,LSTM0
real(4)  :: angle0,lon0,et0,ast0,lst0
contains

subroutine calcu_ast(lst0,N0,lon0,ast0)
implicit none
real(4) :: N0,LSTM0
real(4)  :: angle0,lon0,et0,ast0,lst0
angle0 = 360*(N0-81)/365
et0 = 9.87*sin(2*angle0*(3.14159/180))-7.53*cos(angle0*(3.14159/180))-1.5*sin(angle0*(3.14159/180))
LSTM0 = 15*floor(lon0/15)
ast0 = lst0+(4*(LSTM0-lon0)+et0)/60
!write(*,*)   'cal_AST:',lst0,angle0,et0,LSTM0,ast0,lon0
end subroutine calcu_ast

end module cal_AST


