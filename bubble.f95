Program bubble
!I’m asking the user to type the name of a file. 
!I skip the first line in file because the first line has a sentence. 
! B is used as an index of A. So B is not a real but it is an allocatable integer. 


implicit none

real, allocatable :: A (:)
real :: Temp
integer :: i, j, n
character (len=50) :: InFile


print*, "What is your file?"
read(*,*) InFile !asks the user to type file name
open(42,file=InFile)
read(42,*) ! This skips a line in the file

open (10, file= 'Num1.txt') 
allocate (A (n))

 do i=1, n
   read (10, *) A (i)
 enddo


 do i=1, n
   do j= 1, n-i
      if (A(j) .gt. A (j+1)) then !If one number bigger swap it
         Temp = A (j)
         A(j) = A(j+1)
         A(j+1) = Temp
      endif
   enddo
 enddo   

 !trying to write the bubble sorting into a new file
 open (40, file='Output.txt')
   do i= 1, n
     write (40, *) A(j)
   enddo
   
 deallocate (A)

 Print*, 'Done!'

 close (10)


end program bubble
