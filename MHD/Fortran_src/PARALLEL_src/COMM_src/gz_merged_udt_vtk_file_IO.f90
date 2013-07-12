!gz_merged_udt_vtk_file_IO.f90
!------- module gz_merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine write_gz_merged_ucd_file(istep)
!      subroutine write_gz_merged_udt_file(istep)
!      subroutine write_gz_merged_grd_file
!
!      subroutine write_gz_merged_vtk_file(istep)
!      subroutine write_gz_merged_vtk_phys(istep)
!      subroutine write_gz_merged_vtk_grid
!
      module gz_merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_ucd_data
      use m_field_file_format
      use set_ucd_file_names
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_ucd_file(istep)
!
      use gz_merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(fem_ucd%file_prefix, iflag_ucd_gz,    &
     &      istep, gzip_name)
!
        write(*,*) 'gzipped single UCD data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_ucd_mesh(fem_ucd%nnod, fem_ucd%nele,                 &
     &    fem_ucd%nnod_4_ele, fem_ucd%xx, fem_ucd%ie, fem_ucd%ntot_comp)
!
      call write_merged_gz_ucd_fields(fem_ucd%nnod, fem_ucd%num_field,          &
     &    fem_ucd%ntot_comp, fem_ucd%num_comp, fem_ucd%phys_name, fem_ucd%d_ucd)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_udt_file(istep)
!
      use gz_merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(fem_ucd%file_prefix, iflag_udt_gz,    &
     &      istep, gzip_name)
!
        write(*,*) 'gzipped single UCD field data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_ucd_fields(fem_ucd%nnod, fem_ucd%num_field,          &
     &    fem_ucd%ntot_comp, fem_ucd%num_comp, fem_ucd%phys_name, fem_ucd%d_ucd)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_grd_file
!
      use gz_merged_ucd_data_IO
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(fem_ucd%file_prefix, iflag_udt_gz,    &
     &      gzip_name)
!
        write(*,*) 'gzipped single UCD grid data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_ucd_mesh(fem_ucd%nnod, fem_ucd%nele,                 &
     &    fem_ucd%nnod_4_ele, fem_ucd%xx, fem_ucd%ie, fem_ucd%ntot_comp)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_file(istep)
!
      use gz_merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(fem_ucd%file_prefix, iflag_vtk_gz,    &
     &      istep, gzip_name)
!
        write(*,*) 'gzipped single VTK data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_vtk_mesh(fem_ucd%nnod, fem_ucd%nele,                 &
     &    fem_ucd%nnod_4_ele, fem_ucd%xx, fem_ucd%ie)
!
      call write_merged_gz_vtk_fields(fem_ucd%nnod, fem_ucd%num_field,          &
     &    fem_ucd%ntot_comp, fem_ucd%num_comp, fem_ucd%phys_name, fem_ucd%d_ucd)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_phys(istep)
!
      use gz_merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(fem_ucd%file_prefix, iflag_vtd_gz,    &
     &      istep, gzip_name)
!
        write(*,*) 'gzipped single VTK field data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_vtk_fields(fem_ucd%nnod, fem_ucd%num_field,          &
     &    fem_ucd%ntot_comp, fem_ucd%num_comp, fem_ucd%phys_name, fem_ucd%d_ucd)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_grid
!
      use gz_merged_vtk_data_IO
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(fem_ucd%file_prefix, iflag_vtd_gz,    &
     &      gzip_name)
!
        write(*,*) 'gzipped single VTK grid data:  ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_vtk_mesh(fem_ucd%nnod, fem_ucd%nele,                 &
     &    fem_ucd%nnod_4_ele, fem_ucd%xx, fem_ucd%ie)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module gz_merged_udt_vtk_file_IO
