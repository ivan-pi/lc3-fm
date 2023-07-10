program lc3
use, intrinsic :: iso_fortran_env, only: int8, int16
implicit none

integer, parameter :: word = int16

! Memory
integer(word) :: mem(0:65535) = 0

! Addresses z'0000' to z'2FFF' (inclusive) are reserved for
! vector tables and an operating system. User programs start at z'3000'
!
! The entire address spans 0 (z'0000') to 65535 (z'FFFF').

integer, parameter :: START = int(z'3000')

! Registers
integer(word) :: V(0:9) = [0,0,0,0,0,0,0,0,START,0]

! V0 ... general purpose, usef for reading/writing data
! V1 - V7 general purpose
! V8 ... program counter register (PC), contains memory address of next instruction
! V9 ... conditional registers, contains information about previous operation

! One problem we face in Fortran is the program counter is a 16-bit signed value
! with the range -32768 through 32767 (assuming two complement integers)

integer, parameter :: PC  = 8
integer, parameter :: CND = 9


! Load instructions (LD,LDI,LDR,LEA) and 
! operate instructions (ADD,AND,NOT) each load a result
! into a general purpose register. Based on the result
! the condition code is set,


integer :: d, b, s1, s2, offset
character(1) :: c
character(256) :: file
integer :: ierr



! ----------- STATEMENTS START HERE ---------------------------

nargs = command_argument_count()
if (nargs < 2) then
   write(*,'(A)') "Usage: lc3 <file>"
   stop
end if

call get_command_argument(1,file)

! load program into memory
call load(file,0,ierr)
if (ierr < 0) then
   write(*,'(A)') "Error encountered loading file. Terminating."
   error stop
end if

exec: do

   code = mem(V(PC))    ! Read opcode
   V(PC) = V(PC) + 1  ! Increment program counter

   op = shiftl(code,12)

   select case (op)
   case(0) ! br: Conditional branch

      b = iand(shiftl(code,9),b'111')
      if (iand(b,V(CND))) then
         offset = 
         V(PC) = V(PC) + offset
      end if

   case(1) ! add: Addition

      ! Two modes
      if (btest(opcode,5)) then

         d  =
         s1 = 
         a = 


         ! TODO: check wraparound?
         V(d) = V(s1) + V(s2)

      else
         d  =
         s1 = 
         s2 = 
         ! TODO: check wraparound?
         V(d) = V(s1) + V(s2)
      end if


      V(CND) = flag(V(d))


   case(2) ! ld: Load PC + offset

      d = 
      offset = 

      V(d) = mem(V(PC) + offset)
      ! Set status flag
      V(CND) = flag(V(d))

   case(3) ! st: Store

      s = 
      offset =

      mem(V(PC) + offset) = V(s)


   case(4) ! jsr: Jump to Subroutine

      V(7) = V(PC)

      if (btest(opcode,11)) then
         offset =
         V(PC) = V(PC) + offset 
      else
         b = 
         V(PC) = V(b)
      end if


   case(5) ! and: Bitwise logical and

      if (btest(opcode,5)) then
         ! And 2

         d =
         s1 =
         a =

         V(d) = iand(V(s1),a) 

      else
         ! And 1
         d = 
         s1 = 
         s2 =

         V(d) = iand(V(s1),V(s2))
      end if

      V(CND) = flag(V(d))


   case(6) ! ldr: Load Base + Offset

      d =
      b =
      offset = 

      V(d) = mem(V(b) + offset)
      V(CND) = flag(V(d))

   case(7) ! str: Store base + Offset

      d =
      b =
      offset = 

      mem(V(b) + offset) = V(d)


   case(8) ! rti: Return from interrupt
      ! Not implemented
      ! Has to do with supervisor modes of the processor
      ! Since we only emulate user mode, here we
      ! should launch a privilege mode exception


   case(9) ! not: Bitwise complement

      d = 
      s = 

      V(d) = not(V(s))
      V(CND) = flag(V(d))

   case(10) ! ldi: Load indirect

      d =
      offset = 

      V(d) = mem(mem(V(PC) + offset))
      V(CND) = flag(V(d))

   case(11) ! sti: Store indirect

      s =
      offset = 

      mem(mem(V(PC) + offset)) = V(s)


   case(12) ! jmp: Jump/Return to subroutine

      b = 
      V(PC) = V(b)
      
      ! when b = (111)_2 = (7)_10, this
      ! becomes a return statement


   !case(13) ! res: Reserved for future use
      ! 
      ! Initiate an illegal opcode exception

   case(14) ! lea: Load effective address

      d =
      offset =

      V(d) = V(PC) + offset
      V(CND) = flag(V(d))

   case(15) ! trap: System trap/call

      V(7) = V(PC) ! Store program counter

      ! This instruction would generally launch a system call, to a routine
      ! in the reserved part of memory.
      ! The locations z'0000' through z'00FF', 256 in all, are available
      ! for the starting addresses of systems calls, for the corresponding
      ! trap vectors.

      tf = 

      select case(tf)
      case(0) ! z'20' - GETC
         ! Read character
         read *, c
         V(0) = transfer(c,V(0))
      case(1) ! z'21' - OUT
         ! Write character
         write('(A)',advance='no') transfer(V(0),'a')
      case(2) ! z'22' - PUTS 
         ! Write string to console
      case(3) ! z'23' - IN
         ! Read character from keyboard and echo
         read *, V(0)
         print *, V(0)
      case(4) ! z'24' - PUTSP
         ! Store 2 characters, stop when we reach null
      case(5) ! z'25' - HALT
         ! Halt execution and a print a message on the console
         exit exec
      case(6) 
         ! Read an integer
         read *, V(0)
      case(7) 
         ! Write an integer
         print *, V(0)
      end select

   end select


end do exec

contains

   pure integer(word) function flag(r)
      integer(word), value :: r

      ! 001 ... positive
      ! 010 ... zero
      ! 100 ... negative

      integer, parameter :: FP = 1
      integer, parameter :: FZ = 2
      integer, parameter :: FN = 4

      if (r == 0) then
         flag = FZ
      else if (r > 0) then
         flag = FP
      else
         flag = FN
      end if
   end function


   subroutine load(filename,offset,ierr)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: offset
      integer, intent(out) :: ierr

      logical :: exists
      integer :: sz, hf
      integer :: stat

      integer, parameter :: start = int(z'3000')

      ierr = 0

      inquire(file=trim(filename),exists=,size=sz)
      if (.not. exists) then
         ierr = -1
         return
      end if

      if (sz > 65536 - (start + offset)) then
         write(*,*) "Warning: file exceeds available memory and will be truncated"
         ierr = -2
      end if

      open(newunit=hf,file=file, &
         form="unformatted",
         access="stram",
         action="read",
         status="old")

      lb = start + offset
      ub = min(65535,lb + sz)

      read(hf,iostat=stat) mem(lb:ub)

      close(hf)

   end subroutine

end program
