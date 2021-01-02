subroutine Matrix_multip(M,N,MN)
implicit none
integer :: i,j
real(8),dimension(4,3),intent(in) :: M
real(8),dimension(3,3),intent(in)  :: N
real(8),dimension(4,3),intent(out) :: MN
do i = 1,4
do j = 1,3
    MN(i,j) = M(i,1)*N(1,j)+M(i,2)*N(2,j)+M(i,3)*N(3,j)
end do
end do
end subroutine Matrix_multip


