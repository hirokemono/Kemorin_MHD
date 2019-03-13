!gz_filter_moms_type_file_IO.f90
!      module gz_filter_moms_type_file_IO
!
!     Written by H. Matsui in 2004
!
!      subroutine read_num_filter_mom_t_file_gz(file_name, id_rank,     &
!     &          FEM_elens, FEM_moms)
!      subroutine read_filter_elen_t_file_gz(file_name, id_rank,        &
!     &          nnod, nele, FEM_elens, ierr)
!      subroutine write_filter_elen_t_file_gz(file_name, id_rank,       &
!     &          FEM_elens)
!
!      subroutine read_filter_moms_t_file_gz(file_name, id_rank,        &
!     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!      subroutine write_filter_moms_t_file_gz(file_name, id_rank,       &
!     &          FEM_elens, FEM_moms)
!        character(len=kchara), intent(in) :: file_name
!        integer, intent(in) :: id_rank
!        integer(kind = kint), intent(in) :: nnod, nele
!        type(gradient_model_data_type), intent(in) :: FEM_elens
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!        integer(kind = kint), intent(inout) :: ierr
!
      module gz_filter_moms_type_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_filter_elength
      use set_parallel_file_name
      use skip_gz_comment
      use gz_mesh_data_IO
      use gz_filter_mom_type_data_IO
!
        implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_t_file_gz(file_name, id_rank,      &
     &          FEM_elens, FEM_moms)
!
      use t_filter_moments
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(file_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write gzipped number of filter moms file: ',        &
     &             trim(file_name)
      else if(id_rank .eq. 0) then
        write(*,*) 'Write gzipped number of filter moms files: ',       &
     &             trim(file_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_filter_moment_num_type(FEM_elens, FEM_moms)
      call close_gzfile_f
!
      end subroutine read_num_filter_mom_t_file_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_t_file_gz(file_name, id_rank,         &
     &          nnod, nele, FEM_elens, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(file_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter length file: ',                 &
     &             trim(file_name)
      else if(id_rank .eq. 0) then
        write(*,*) 'Read gzipped filter length files: ',                &
     &             trim(file_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_filter_elen_data_type(nnod, nele, FEM_elens, ierr)
      call close_gzfile_f
!
      end subroutine read_filter_elen_t_file_gz
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_t_file_gz(file_name, id_rank,        &
     &          FEM_elens)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(file_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write gzipped filter length file: ',                &
     &             trim(file_name)
      else if(id_rank .eq. 0) then
        write(*,*) 'Write gzipped filter length files: ',               &
     &             trim(file_name)
      end if
!
      call open_wt_gzfile_f(gzip_name)
      call gz_write_filter_elen_data_type(FEM_elens)
      call close_gzfile_f
!
      end subroutine write_filter_elen_t_file_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moms_t_file_gz(file_name, id_rank,         &
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
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(file_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter moment file: ',                 &
     &             trim(file_name)
      else if(id_rank .eq. 0) then
        write(*,*) 'Read gzipped filter moment files: ',                &
     &             trim(file_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_filter_moms_data_type(nnod, nele,                    &
     &    FEM_elens, FEM_moms, ierr)
      call close_gzfile_f
!
      end subroutine read_filter_moms_t_file_gz
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moms_t_file_gz(file_name, id_rank,        &
     &          FEM_elens, FEM_moms)
!
      use t_filter_moments
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(file_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write gzipped filter moment file: ',                &
     &             trim(file_name)
      else if(id_rank .eq. 0) then
        write(*,*) 'Write gzipped filter moment files: ',               &
     &             trim(file_name)
      end if
!
      call open_wt_gzfile_f(gzip_name)
      call gz_write_filter_moms_data_type(FEM_elens, FEM_moms)
      call close_gzfile_f
!
      end subroutine write_filter_moms_t_file_gz
!
!-----------------------------------------------------------------------
!
      end module gz_filter_moms_type_file_IO
