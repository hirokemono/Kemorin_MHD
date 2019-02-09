!>@file  field_data_IO_b.f90
!!       module field_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_data_b                                    &
!!     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO)
!!      subroutine write_field_data_b(nnod, num_field, ntot_comp,       &
!!     &          ncomp_field, field_name, d_nod)
!!
!!      subroutine read_step_data_b(iflag_swap,                         &
!!     &          i_time_step_IO, time_IO, delta_t_IO,                  &
!!     &          istack_merged, num_field, ierr)
!!      subroutine read_field_data_b(iflag_swap,                        &
!!     &          nnod, num_field, ntot_comp, field_name, vect, ierr)
!!@endverbatim
!
      module field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_step_data_b                                      &
     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO)
!
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
!
      call write_one_integer_b(id_rank)
      call write_one_integer_b(i_time_step_IO)
!
      call write_one_real_b(time_IO)
      call write_one_real_b(delta_t_IO)
!
      end subroutine write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_b(nnod, num_field, ntot_comp,         &
     &          ncomp_field, field_name, d_nod)
!
      use m_phys_constants
!
      integer(kind=kint), intent(in) :: nnod, num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      istack_merged(1) = nnod
      call write_mul_int8_b(ione, istack_merged)
      call write_one_integer_b(num_field)
      call write_mul_integer_b(num_field, ncomp_field)
!
      call write_mul_character_b(num_field, field_name)
      call write_2d_vector_b(nnod, ntot_comp, d_nod)
!
      end subroutine write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_b(iflag_swap,                           &
     &          i_time_step_IO, time_IO, delta_t_IO,                    &
     &          istack_merged, num_field, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
!
      integer(kind=kint_gl), intent(inout) :: istack_merged(1)
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: id_rank
!
!
      call read_one_integer_b(iflag_swap, id_rank, ierr)
      if(ierr .gt. 0) return
      call read_one_integer_b(iflag_swap, i_time_step_IO, ierr)
      if(ierr .gt. 0) return
      call read_one_real_b(iflag_swap, time_IO, ierr)
      if(ierr .gt. 0) return
      call read_one_real_b(iflag_swap, delta_t_IO, ierr)
      if(ierr .gt. 0) return
!
      call read_mul_int8_b(iflag_swap, ione, istack_merged, ierr)
      if(ierr .gt. 0) return
!
      call read_one_integer_b(iflag_swap, num_field, ierr)
!
      end subroutine read_step_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_b(iflag_swap,                          &
     &          nnod, num_field, ntot_comp, field_name, vect, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ntot_comp)
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_mul_character_b(num_field, field_name, ierr)
      if(ierr .gt. 0) return
!
      call read_2d_vector_b(iflag_swap, nnod, ntot_comp, vect, ierr)
!
      end subroutine read_field_data_b
!
! -----------------------------------------------------------------------
!
      end module field_data_IO_b
