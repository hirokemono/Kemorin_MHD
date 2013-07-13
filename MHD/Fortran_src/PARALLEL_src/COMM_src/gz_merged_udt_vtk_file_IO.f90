!gz_merged_udt_vtk_file_IO.f90
!------- module gz_merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine write_gz_merged_ucd_file(istep, ucd, m_ucd)
!      subroutine write_gz_merged_udt_file(istep, ucd, m_ucd)
!      subroutine write_gz_merged_grd_file(ucd, m_ucd)
!
!      subroutine write_gz_merged_vtk_file(istep, ucd, m_ucd)
!      subroutine write_gz_merged_vtk_phys(istep, ucd, m_ucd)
!      subroutine write_gz_merged_vtk_grid(ucd, m_ucd)
!
      module gz_merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_field_file_format
!
      use t_ucd_data
!
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
      subroutine write_gz_merged_ucd_file(istep, ucd, m_ucd)
!
      use gz_merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix,                  &
     &      iflag_ucd_gz, istep, gzip_name)
!
        write(*,*) 'gzipped single UCD data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_ucd_mesh(ucd%nnod, ucd%nele, ucd%nnod_4_ele, &
     &    ucd%xx, ucd%ie, ucd%ntot_comp, m_ucd%istack_merged_nod,       &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call write_merged_gz_ucd_fields(ucd%nnod, ucd%num_field,          &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd,        &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_udt_file(istep, ucd, m_ucd)
!
      use gz_merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix,                  &
     &      iflag_udt_gz, istep, gzip_name)
!
        write(*,*) 'gzipped single UCD field data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_ucd_fields(ucd%nnod, ucd%num_field,          &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd,        &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_grd_file(ucd, m_ucd)
!
      use gz_merged_ucd_data_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd%file_prefix, iflag_udt_gz,    &
     &      gzip_name)
!
        write(*,*) 'gzipped single UCD grid data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_ucd_mesh(ucd%nnod, ucd%nele, ucd%nnod_4_ele, &
     &    ucd%xx, ucd%ie, ucd%ntot_comp, m_ucd%istack_merged_nod,       &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_file(istep, ucd, m_ucd)
!
      use gz_merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_vtk_gz,    &
     &      istep, gzip_name)
!
        write(*,*) 'gzipped single VTK data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_vtk_mesh(ucd%nnod, ucd%nele, ucd%nnod_4_ele, &
     &    ucd%xx, ucd%ie,  m_ucd%istack_merged_nod,                     &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call write_merged_gz_vtk_fields(ucd%nnod, ucd%num_field,          &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd,        &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_phys(istep, ucd, m_ucd)
!
      use gz_merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_vtd_gz,    &
     &      istep, gzip_name)
!
        write(*,*) 'gzipped single VTK field data: ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_vtk_fields(ucd%nnod, ucd%num_field,          &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd,        &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_grid(ucd, m_ucd)
!
      use gz_merged_vtk_data_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd%file_prefix, iflag_vtd_gz,    &
     &      gzip_name)
!
        write(*,*) 'gzipped single VTK grid data:  ', trim(gzip_name)
        call open_wt_gzfile(gzip_name)
      end if
!
      call write_merged_gz_vtk_mesh(ucd%nnod, ucd%nele, ucd%nnod_4_ele, &
     &    ucd%xx, ucd%ie,  m_ucd%istack_merged_nod,                     &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      if(my_rank .eq. 0) call close_gzfile
!
      end subroutine write_gz_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module gz_merged_udt_vtk_file_IO
