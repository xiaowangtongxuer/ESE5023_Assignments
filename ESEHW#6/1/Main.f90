program Main
implicit none
  integer :: u,i,j
  real(8), dimension (4,3) :: M
  real(8), dimension (3,3) :: N
  real(8), dimension (4,3) :: MN
u=50
open(u,file='M.dat',status='old')
   do i=1,4
     read(u,*) (M(i,j),j=1,3)
   enddo
close(u)
open(u,file='N.dat',status='old')
   do i=1,3
     read(u,*) (N(i,j),j=1,3)
   enddo
close(u)

call Matrix_multip(M,N,MN)
open(unit=u,file='MN.dat',status='replace')
write(u,"(f8.1,f8.1,f8.1)") MN(1,1),MN(1,2),MN(1,3)
write(u,"(f8.1,f8.1,f8.1)") MN(2,1),MN(2,2),MN(2,3)
write(u,"(f8.1,f8.1,f8.1)") MN(3,1),MN(3,2),MN(3,3)
write(u,"(f8.1,f8.1,f8.1)") MN(4,1),MN(4,2),MN(4,3)
close(u)
end program Main

! good work!
