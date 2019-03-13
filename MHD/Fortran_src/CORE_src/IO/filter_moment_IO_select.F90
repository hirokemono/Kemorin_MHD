!>@file   filter_moment_IO_select.f90
!!@brief  module filter_moment_IO_select
!!
!!@author H. Matsui
!!@date Programmed in 2004
!
!> @brief Filter data file IO selector
!!
!!@verbatim
!!      subroutine sel_read_sort_filter_coef_file                       &
!!     &         (id_rank, filter_IO, ierr)
!!      subroutine sel_write_sort_filter_coef_file(id_rank, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!      subroutine sel_read_filter_geometry_file                        &
!!     &          (id_rank, filter_IO, ierr)
!!      subroutine sel_write_filter_geometry_file(id_rank, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!
!!      subroutine sel_read_num_filter_mom_file(id_rank,                &
!!     &          FEM_elens, FEM_moms, ierr)
!!      subroutine sel_read_filter_elen_file(id_rank, nnod, nele,       &
!!     &          FEM_elens, ierr)
!!      subroutine sel_write_filter_elen_file(id_rank, FEM_elens)
!!      subroutine sel_read_filter_moms_file(id_rank, nnod, nele,       &
!!     &          FEM_elens, FEM_moms, ierr)
!!      subroutine sel_write_filter_moms_file(id_rank,                  &
!!     &          FEM_elens, FEM_moms)
!!          integer, intent(in) :: id_rank
!!          integer(kind = kint), intent(in) :: nnod, nele
!!          type(gradient_model_data_type), intent(inout) :: FEM_elens
!!          type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!!          integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
!
      module filter_moment_IO_select
!
      use m_precision
!
      use m_filter_file_names
      use m_file_format_switch
      use t_filter_file_data
      use t_filter_coefficients
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_sort_filter_coef_file                         &
     &         (id_rank, filter_IO, ierr)
!
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use gz_filter_coefs_file_IO
!
      integer, intent(in) :: id_rank
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name =  add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_sorted_filter_coef_file_b                             &
     &     (file_name, id_rank, filter_IO, ierr)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_sort_filter_coef_file_gz                              &
     &     (file_name, id_rank, filter_IO, ierr)
        return
      end if
#endif
!
        call read_sorted_filter_coef_file                               &
     &     (file_name, id_rank, filter_IO, ierr)
!
      end subroutine sel_read_sort_filter_coef_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_sort_filter_coef_file(id_rank, filter_IO)
!
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use gz_filter_coefs_file_IO
!
      integer, intent(in) :: id_rank
      type(filter_file_data), intent(inout) :: filter_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
!
      if (ifmt_filter_file .eq. id_ascii_file_fmt) then
        call write_sorted_filter_coef_file                              &
     &     (file_name, id_rank, filter_IO)
#ifdef ZLIB_IO
      else if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call write_sorted_filter_coef_file_b                            &
     &     (file_name, id_rank, filter_IO)
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call write_sort_filter_coef_file_gz                             &
     &     (file_name, id_rank, filter_IO)
#endif
!
      else
        call write_sorted_filter_coef_file                              &
     &     (file_name, id_rank, filter_IO)
      end if
!
      call dealloc_3d_filter_weight(filter_IO%filters)
      call dealloc_3d_filter_function(filter_IO%filters)
      call dealloc_inod_filter_weights(filter_IO%filters)
      call dealloc_num_filtering_comb(filter_IO%filters)
!
      end subroutine sel_write_sort_filter_coef_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_filter_geometry_file                          &
     &         (id_rank, filter_IO, ierr)
!
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use gz_filter_coefs_file_IO
!
      integer, intent(in) :: id_rank
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_filter_geometry_file_b                                &
     &     (file_name, id_rank, filter_IO, ierr)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_filter_geometry_file_gz                               &
     &     (file_name, id_rank, filter_IO, ierr)
        return
      end if
#endif
!
      call read_filter_geometry_file                                    &
     &   (file_name, id_rank, filter_IO, ierr)
!
      end subroutine sel_read_filter_geometry_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_filter_geometry_file(id_rank, filter_IO)
!
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use gz_filter_coefs_file_IO
!
      integer, intent(in) :: id_rank
      type(filter_file_data), intent(inout) :: filter_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call write_filter_geometry_file_b                               &
     &     (file_name, id_rank, filter_IO)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call write_filter_geometry_file_gz                              &
     &     (file_name, id_rank, filter_IO)
        return
      end if
#endif
!
      call write_filter_geometry_file(file_name, id_rank, filter_IO)
!
      end subroutine sel_write_filter_geometry_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_num_filter_mom_file(id_rank,                  &
     &          FEM_elens, FEM_moms, ierr)
!
      use t_filter_elength
      use t_filter_moments
      use filter_moments_type_file_IO
      use filter_moments_file_IO_b
      use gz_filter_moms_type_file_IO
!
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_num_filter_mom_type_file_b(file_name, id_rank,        &
      &     FEM_elens, FEM_moms, ierr)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_num_filter_mom_t_file_gz(file_name, id_rank,          &
     &      FEM_elens, FEM_moms)
        return
       end if
#endif
!
        call read_num_filter_mom_type_file(file_name, id_rank,          &
      &     FEM_elens, FEM_moms)
!
      end subroutine sel_read_num_filter_mom_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_filter_elen_file(id_rank, nnod, nele,         &
     &          FEM_elens, ierr)
!
      use t_filter_elength
      use filter_moments_type_file_IO
      use filter_moments_file_IO_b
      use gz_filter_moms_type_file_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_filter_elen_type_file_b(file_name, id_rank,           &
     &      nnod, nele, FEM_elens, ierr)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_filter_elen_t_file_gz(file_name, id_rank, nnod, nele, &
     &      FEM_elens, ierr)
        return
      end if
#endif
!
        call read_filter_elen_type_file(file_name, id_rank,             &
     &      nnod, nele, FEM_elens, ierr)
!
      end subroutine sel_read_filter_elen_file
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_filter_elen_file(id_rank, FEM_elens)
!
      use t_filter_elength
      use filter_moments_type_file_IO
      use filter_moments_file_IO_b
      use gz_filter_moms_type_file_IO
!
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call write_filter_elen_type_file_b(file_name, id_rank,          &
     &      FEM_elens)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call write_filter_elen_t_file_gz(file_name, id_rank, FEM_elens)
        return
      end if
#endif
!
        call write_filter_elen_type_file(file_name, id_rank, FEM_elens)
!
      end subroutine sel_write_filter_elen_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_filter_moms_file(id_rank, nnod, nele,        &
     &          FEM_elens, FEM_moms, ierr)
!
      use t_filter_elength
      use t_filter_moments
      use filter_moments_type_file_IO
      use filter_moments_file_IO_b
      use gz_filter_moms_type_file_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_filter_moms_type_file_b(file_name, id_rank,           &
     &      nnod, nele, FEM_elens, FEM_moms, ierr)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_filter_moms_t_file_gz(file_name, id_rank, nnod, nele, &
     &      FEM_elens, FEM_moms, ierr)
        return
      end if
#endif
!
        call read_filter_moms_type_file(file_name, id_rank,             &
     &      nnod, nele, FEM_elens, FEM_moms, ierr)
!
      end subroutine sel_read_filter_moms_file
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_filter_moms_file(id_rank,                    &
     &          FEM_elens, FEM_moms)
!
      use t_filter_elength
      use t_filter_moments
      use filter_moments_type_file_IO
      use filter_moments_file_IO_b
      use gz_filter_moms_type_file_IO
!
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      character(len=kchara) :: file_name
!
!
      file_name = add_int_suffix(id_rank, filter_file_head)
!
#ifdef ZLIB_IO
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call write_filter_moms_type_file_b(file_name, id_rank,          &
     &      FEM_elens, FEM_moms)
        return
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call write_filter_moms_t_file_gz(file_name, id_rank,            &
     &      FEM_elens, FEM_moms)
        return
      end if
#endif
!
        call write_filter_moms_type_file(file_name, id_rank,            &
     &      FEM_elens, FEM_moms)
!
      end subroutine sel_write_filter_moms_file
!
!-----------------------------------------------------------------------
!
      end module filter_moment_IO_select
