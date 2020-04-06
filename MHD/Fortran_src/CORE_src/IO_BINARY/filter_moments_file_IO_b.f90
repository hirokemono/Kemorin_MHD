!>@file   filter_moments_file_IO_b.f90
!!@brief  module filter_moments_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in 2004
!!@n     modified by H. Matsui in May., 2008
!
!> @brief file IO for filtering structure
!!
!!@verbatim
!!      subroutine read_num_filter_mom_type_file_b(file_name, id_rank,  &
!!     &          FEM_elens, FEM_moms, ierr)
!!
!!      subroutine read_filter_elen_type_file_b(file_name, id_rank,     &
!!     &          nnod, nele, FEM_elens, ierr)
!!      subroutine write_filter_elen_type_file_b(file_name, id_rank,    &
!!     &          FEM_elens, ierr)
!!
!!      subroutine read_filter_moms_type_file_b(file_name, id_rank,     &
!!     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!!      subroutine write_filter_moms_type_file_b(file_name, id_rank,    &
!!     &          FEM_elens, FEM_moms, ierr)
!!        character(len=kchara), intent(in) :: file_name
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: nnod, nele
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module filter_moments_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_filter_elength
      use m_file_format_switch
      use filter_mom_type_data_IO_b
      use set_parallel_file_name
      use binary_IO
!
      implicit none
!
      type(binary_IO_flags), private :: bin_fmflags
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_type_file_b(file_name, id_rank,    &
     &          FEM_elens, FEM_moms, ierr)
!
      use t_filter_moments
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary number of filter moms file: ',         &
     &             trim(file_name)
      end if
!
      call open_read_binary_file(file_name, id_rank, bbuf1)
      if(bbuf1%ierr_bin .ne. 0) goto 99
      call read_filter_moment_num_b(bbuf1, FEM_elens, FEM_moms)
      if(bbuf1%ierr_bin .gt. 0) goto 99
!
  99  continue
      call close_binary_file
      ierr = bbuf1%ierr_bin
!
      end subroutine read_num_filter_mom_type_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_type_file_b(file_name, id_rank,       &
     &          nnod, nele, FEM_elens, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter length file: ',                  &
     &             trim(file_name)
      end if
!
      call open_read_binary_file(file_name, id_rank, bbuf1)
      if(bbuf1%ierr_bin .ne. 0) goto 99
      call read_filter_elen_data_b(nnod, nele, bbuf1, FEM_elens)
!
  99  continue
      call close_binary_file
      ierr = bbuf1%ierr_bin
!
      end subroutine read_filter_elen_type_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_type_file_b(file_name, id_rank,      &
     &          FEM_elens, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter length file: ',                 &
     &             trim(file_name)
      end if
!
      call open_write_binary_file(file_name, bbuf1)
      if(bbuf1%ierr_bin .gt. 0) go to 99
      call write_filter_elen_data_b(FEM_elens, bbuf1)
!
  99  continue
      call close_binary_file
      ierr = bbuf1%ierr_bin
!
      end subroutine write_filter_elen_type_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moms_type_file_b(file_name, id_rank,       &
     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!
      use t_filter_moments
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter moment file: ',                  &
     &             trim(file_name)
      end if
!
      call open_read_binary_file(file_name, id_rank, bbuf1)
      if(bbuf1%ierr_bin .ne. 0) goto 99
      call read_filter_moms_data_b                                      &
     &   (nnod, nele, bbuf1, FEM_elens, FEM_moms)
!
  99  continue
      call close_binary_file
      ierr = bbuf1%ierr_bin
!
      end subroutine read_filter_moms_type_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moms_type_file_b(file_name, id_rank,      &
     &          FEM_elens, FEM_moms, ierr)
!
      use t_filter_moments
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter moment file: ',                 &
     &             trim(file_name)
      end if
!
      call open_write_binary_file(file_name, bbuf1)
      if(bbuf1%ierr_bin .gt. 0) go to 99
      call write_filter_moms_data_b                                     &
     &   (FEM_elens, FEM_moms, bbuf1)
!
  99  continue
      call close_binary_file
      ierr = bbuf1%ierr_bin
!
      end subroutine write_filter_moms_type_file_b
!
!-----------------------------------------------------------------------
!
      end module filter_moments_file_IO_b
