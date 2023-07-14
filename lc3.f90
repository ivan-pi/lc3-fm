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


   ! Bits [15:12] contain the opcode
   op = ibits(code,12,4)

   select case (op)
   case(0) ! br: Conditional branch

      b = ibits(code,9,3)

      if (iand(b,V(CND)) > 0) then
         offset = ibits(code,0,9)
         V(PC) = V(PC) + sext(offset,9)
      end if

   case(1) ! add: Addition

      d  = ibits(code,9,3)
      s1 = ibits(code,6,3)

      ! Two modes
      if (btest(code,5)) then
         imm  = ibits(code,0,5)
         V(d) = V(s1) + sext(imm,5)
      else
         s2 = ibits(code,0,3) 
         V(d) = V(s1) + V(s2)
      end if

      V(CND) = flag(V(d))

   case(2) ! ld: Load PC + offset

      d = ibits(code,9,3)
      offset = ibits(code,0,9)

      V(d) = mem(V(PC) + sext(offset,9))
      ! Set status flag
      V(CND) = flag(V(d))

   case(3) ! st: Store

      s = ibits(code,9,3)
      offset = ibits(code,0,9)

      mem(V(PC) + sext(offset,9)) = V(s)


   case(4) ! jsr: Jump to Subroutine

      V(7) = V(PC)

      if (btest(opcode,11)) then
         offset = ibits(code,0,11)
         V(PC) = V(PC) + sext(offset,11) 
      else
         b = ibits(code,6,3)
         V(PC) = V(b)
      end if

   case(5) ! and: Bitwise logical and

      if (btest(code,5)) then
         ! And 2

         d  = ibits(code,9,3)
         s1 = ibits(code,6,3)
         a  = ibits(code,0,5)

         V(d) = iand(V(s1),sext(a,5)) 

      else
         ! And 1
         d  = ibits(code,9,3)
         s1 = ibits(code,6,3)
         s2 = ibits(code,0,3)

         V(d) = iand(V(s1),V(s2))
      end if

      V(CND) = flag(V(d))


   case(6) ! ldr: Load Base + Offset

      d = ibits(code,9,3)
      b = ibits(code,6,3)
      offset = ibits(code,0,6)

      V(d) = mem(V(b) + sext(offset,6))
      V(CND) = flag(V(d))

   case(7) ! str: Store base + Offset

      s = ibits(code,9,3)
      b = ibits(code,6,3)
      offset = ibits(code,0,6)

      mem(V(b) + sext(offset,6)) = V(s)


   case(8) ! rti: Return from interrupt
      ! Not implemented
      ! Has to do with supervisor modes of the processor
      ! Since we only emulate user mode, here we
      ! should launch a privilege mode exception
      call exception()

   case(9) ! not: Bitwise complement

      d = ibits(code,9,3)
      s = ibits(code,6,3)

      V(d) = not(V(s))
      V(CND) = flag(V(d))

   case(10) ! ldi: Load indirect

      d = ibits(code,9,3)
      offset = ibits(code,0,9)

      V(d) = mem(mem(V(PC) + sext(offset,9)))
      V(CND) = flag(V(d))

   case(11) ! sti: Store indirect

      s = ibits(code,9,3)
      offset = ibits(code,0,9)

      mem(mem(V(PC) + sext(offset,9))) = V(s)


   case(12) ! jmp: Jump/Return to subroutine

      b = ibits(6,3) 
      V(PC) = V(b)
      
      ! when b = (111)_2 = (7)_10, this
      ! becomes a return statement, because when a subroutine
      ! is called with JSR, the PC is stored in V(7)

   case(13) ! res: Reserved for future use
      ! 
      ! Initiate an illegal opcode exception
      call illegal()

   case(14) ! lea: Load effective address

      d = ibits(code,9,3)
      offset = ibits(code,0,9)

      V(d) = V(PC) + sext(offset,9)
      V(CND) = flag(V(d))

   case(15) ! trap: System trap/call

      V(7) = V(PC) ! Store program counter

      ! This instruction would generally launch a system call, to a routine
      ! in the reserved part of memory.
      ! The locations z'0000' through z'00FF', 256 in all, are available
      ! for the starting addresses of systems calls, for the corresponding
      ! trap vectors.
      !
      ! Trap vectors z'20' to z'25' are implemented using service routines

      tf = ibits(code,0,8) ! zero extension automatic
      tf = tf - int(z'20')

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

   function get_pc() 
      integer(word), pointer :: get_pc
      get_pc => V(8)
   end function

   ! Sign-extend the lowest n bits of x
   pure function sext(x,n)
   integer(int16), value :: x, n
   integer(int16) :: sext
   sext = shiftl(ibits(x,0,n),bit_size(x)-n)
   sext = shifta(sext,bit_size(x)-n)
   end function

   ! TODO: Specialize the routine for the specific offsets which
   !       are needed.


   ! Returns the condition code for the register value r
   !
   ! Used with the load instructions (LD, LDI, LDR) and the operate
   ! instructions (ADD, AND, and NOT). Based on wheter the result
   ! is negative, zero, or positive, sets the condition register V(9).
   pure integer(word) function flag(r)
      integer(word), value :: r

      integer, parameter :: FP = 1  ! 001 ... positive
      integer, parameter :: FZ = 2  ! 010 ... zero
      integer, parameter :: FN = 4  ! 100 ... negative

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


   subroutine trap_service(trap)
   integer, intent(in) :: trap

   integer, parameter :: GETC  = int(z'20')
   integer, parameter :: OUT   = int(z'21')
   integer, parameter :: PUTS  = int(z'22')
   integer, parameter :: IN    = int(z'23')
   integer, parameter :: PUTSP = int(z'24')
   integer, parameter :: HALT  = int(z'25')

   select case(trap)
   case(GETC)
      ! Read a single character from the keyboard. The character is not echoed onto
      ! the console. Its ASCII code is copied into R0. The high eight bits of R0 are cleared.
   case(OUT)
      ! Write a character in R0[7:0] to the console display.
   case(PUTS)
      ! Write a string of ASCII characters to the console display. The characters are
      ! contained in consecutive memory locations, one character per memory location,
      ! starting with the address specified in R0. Writing terminates with the occurrence of
      ! z'0000' in a memory location.
   case(IN)
      ! Print a prompt on the screen and read a single character from the keyboard. The
      ! character is exhoed onto the console monitor, and its ASCII code is copied into
      ! R0. The high eight bits of R0 are cleared.
   case(PUTSP)
      ! Write a string of ASCII characters to the console. The characters are contained in
      ! consecutive memory locations, two characters per memory location, starting with
      ! the address specified in R0. The ASCII code contained in bits [7:0] of a memory
      ! location is written to the console first. Then the ASCII code contained in bits [15:8]
      ! of that memory location is written to the console. (A character string consisting of
      ! an odd number of characters to be written will have z'00' in bits[15:8] of the
      ! memory location containing the last character to be written.) Writing terminates
      ! with the occurence of z'0000' in a memory location.
   case(HALT)
      ! Halt execution and print a message on the console
      stop "Halting execution."
   end select

   end subroutine

end program
