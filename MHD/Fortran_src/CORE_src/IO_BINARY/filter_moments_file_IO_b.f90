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
      call open_read_binary_file(file_name, id_rank, bin_fmflags)
      call read_filter_moment_num_type_b                                &
     &   (bin_fmflags, FEM_elens, FEM_moms)
      call close_binary_file
      ierr = bin_fmflags%ierr_IO
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
      call open_read_binary_file(file_name, id_rank, bin_fmflags)
      call read_filter_elen_data_type_b                                 &
     &   (nnod, nele, bin_fmflags, FEM_elens)
      call close_binary_file
      ierr = bin_fmflags%ierr_IO
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
      call open_write_binary_file(file_name, bin_fmflags)
      if(bin_fmflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_filter_elen_data_type_b(FEM_elens, bin_fmflags)
      if(bin_fmflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
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
      call open_read_binary_file(file_name, id_rank, bin_fmflags)
      call read_filter_moms_data_type_b                                 &
     &   (nnod, nele, bin_fmflags, FEM_elens, FEM_moms)
      call close_binary_file
      ierr = bin_fmflags%ierr_IO
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
      call open_write_binary_file(file_name, bin_fmflags)
      if(bin_fmflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_filter_moms_data_type_b                                &
     &   (FEM_elens, FEM_moms, bin_fmflags)
      if(bin_fmflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
!
      end subroutine write_filter_moms_type_file_b
!
!-----------------------------------------------------------------------
!
      end module filter_moments_file_IO_b
