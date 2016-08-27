!>@file  field_data_IO_b.f90
!!       module field_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_data_b(id_rank)
!!      subroutine write_field_data_b(id_rank, nnod, num_field,         &
!!     &          ntot_comp, ncomp_field, field_name, d_nod)
!!
!!      subroutine read_step_data_b(my_rank, istack_merged, num_field)
!!      subroutine read_field_data_b                                    &
!!     &         (nnod, num_field, ncomp, field_name, vect)
!!
!!      subroutine write_endian_flag
!!      subroutine write_fld_inthead_b(int_dat)
!!      subroutine write_fld_realhead_b(real_dat)
!!      subroutine write_fld_mul_i8head_b(num, int_gl_dat)
!!      subroutine write_fld_mul_inthead_b(num, int_dat)
!!      subroutine write_fld_mul_charhead_b(num, chara_dat)
!!      subroutine write_fld_realarray2_b(n1, n2, real_dat)
!!
!!      subroutine read_endian_flag(my_rank)
!!      subroutine read_fld_inthead_b(int_dat)
!!      subroutine read_fld_realhead_b(real_dat)
!!      subroutine read_fld_mul_i8head_b(num, int_gl_dat)
!!      subroutine read_fld_mul_inthead_b(num, int_dat)
!!      subroutine read_fld_mul_charhead_b(num, chara_dat)
!!      subroutine read_fld_realarray2_b(n1, n2, real_dat)
!!@endverbatim
!
      module field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_time_data_IO
      use t_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_step_data_b(id_rank)
!
!
      integer(kind=kint), intent(in) :: id_rank
!
!
      call write_endian_flag
      call write_fld_inthead_b(id_rank)
      call write_fld_inthead_b(i_time_step_IO)
!
      call write_fld_realhead_b(time_IO)
      call write_fld_realhead_b(delta_t_IO)
!
      end subroutine write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_b(id_rank, nnod, num_field,           &
     &          ntot_comp, ncomp_field, field_name, d_nod)
!
      use m_phys_constants
!
!
      integer(kind=kint), intent(in) :: id_rank, nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      call write_step_data_b(id_rank)
!
      istack_merged(1) = nnod
      call write_fld_mul_i8head_b(ione, istack_merged)
      call write_fld_inthead_b(num_field)
      call write_fld_mul_inthead_b(num_field, ncomp_field)
!
      call write_fld_mul_charhead_b(num_field, field_name)
      call write_fld_realarray2_b(nnod, ntot_comp, d_nod)
!
      end subroutine write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_b(my_rank, istack_merged, num_field)
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind=kint_gl), intent(inout) :: istack_merged(1)
      integer(kind=kint), intent(inout) :: num_field
!
      integer(kind = kint) :: id_rank
!
!
      call read_endian_flag(my_rank)
!
      call read_fld_inthead_b(id_rank)
      call read_fld_inthead_b(i_time_step_IO)
      call read_fld_realhead_b(time_IO)
      call read_fld_realhead_b(delta_t_IO)
!
      call read_fld_mul_i8head_b(ione, istack_merged)
      call read_fld_inthead_b(num_field)
!
      end subroutine read_step_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_b                                      &
     &         (nnod, num_field, ncomp, field_name, vect)
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      call read_fld_mul_charhead_b(num_field, field_name)
      call read_fld_realarray2_b(nnod, ncomp, vect)
!
      end subroutine read_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_endian_flag
!
      integer(kind = kint) :: ierr
!
!
      call rawwrite_f(kint, i_UNIX, ierr)
!
      end subroutine write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine write_fld_inthead_b(int_dat)
!
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: ierr
!
!
      call rawwrite_f(kint, int_dat, ierr)
!
      end subroutine write_fld_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine write_fld_realhead_b(real_dat)
!
      real(kind = kreal), intent(in) :: real_dat
!
      integer(kind = kint) :: ierr
!
!
      call rawwrite_f(kreal, real_dat, ierr)
!
      end subroutine write_fld_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_fld_mul_i8head_b(num, int_gl_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int_gl_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kint_gl
      call rawwrite_f(ilength, int_gl_dat(1), ierr)
!
      end subroutine write_fld_mul_i8head_b
!
! -----------------------------------------------------------------------
!
      subroutine write_fld_mul_inthead_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kint
      call rawwrite_f(ilength, int_dat(1), ierr)
!
      end subroutine write_fld_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine write_fld_mul_charhead_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kchara
      call rawwrite_f(ilength, chara_dat(1), ierr)
!
      end subroutine write_fld_mul_charhead_b
!
! -----------------------------------------------------------------------
!
      subroutine write_fld_realarray2_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = n1 * n2 * kreal
      call rawwrite_f(ilength, real_dat(1,1), ierr)
!
      end subroutine write_fld_realarray2_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_endian_flag(my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind = kint) :: int_dat
      integer(kind = kint) :: ierr
!
!
      call rawread_f(iflag_endian, kint, int_dat, ierr)
!
      if(int_dat .eq. i_UNIX) then
        if(my_rank.eq.0) write(*,*) 'binary data have correct endian!'
        iflag_endian = iendian_KEEP
      else if(int_dat .eq. i_XINU) then
        if(my_rank.eq.0) write(*,*) 'binary data have opposite endian!'
        iflag_endian = iendian_FLIP
      else
        iflag_endian = -1
        if(my_rank.eq.0) write(*,*) 'Binary Data is someting wrong!',   &
     &                               int_dat
      end if
!
      end subroutine read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_inthead_b(int_dat)
!
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: ierr
!
!
      call rawread_f(iflag_endian, kint, int_dat, ierr)
!
      end subroutine read_fld_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_realhead_b(real_dat)
!
      real(kind = kreal), intent(inout) :: real_dat
!
      integer(kind = kint) :: ierr
!
!
      call rawread_f(iflag_endian, kreal, real_dat, ierr)
!
      end subroutine read_fld_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_fld_mul_i8head_b(num, int_gl_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int_gl_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kint_gl
      call rawread_f(iflag_endian, ilength, int_gl_dat(1), ierr)
!
      end subroutine read_fld_mul_i8head_b
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_mul_inthead_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kint
      call rawread_f(iflag_endian, ilength, int_dat(1), ierr)
!
      end subroutine read_fld_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_mul_charhead_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kchara
      call rawread_f(iflag_endian, ilength, chara_dat, ierr)
!
      end subroutine read_fld_mul_charhead_b
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_realarray2_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  n1 * n2 * kreal
      call rawread_f(iflag_endian, ilength, real_dat, ierr)
!
      end subroutine read_fld_realarray2_b
!
! -----------------------------------------------------------------------
!
      end module field_data_IO_b
