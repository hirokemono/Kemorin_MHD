!merged_udt_vtk_file_IO.f90
!------- module merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine init_merged_ucd(ucd)
!      subroutine finalize_merged_ucd(ucd)
!
!      subroutine write_merged_ucd_file(istep, ucd)
!      subroutine write_merged_udt_file(istep, ucd)
!      subroutine write_merged_grd_file(ucd)
!
!      subroutine write_merged_vtk_file(istep, ucd)
!      subroutine write_merged_vtk_phys(istep, ucd)
!      subroutine write_merged_vtk_grid(ucd)
!
      module merged_udt_vtk_file_IO
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
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_merged_ucd(ucd)
!
      use m_geometry_parameter
      use m_nod_comm_table
      use m_merged_ucd_data
      use hdf5_file_IO
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call allocate_merged_ucd_num
      call count_merged_ucd(numnod, internal_node, ucd%nele)
!
      call allocate_merged_ucd_data(numnod,                             &
     &    ucd%nnod_4_ele, ucd%ntot_comp)
      call set_node_double_address                                      &
     &         (num_neib, id_neib, istack_import, item_import,          &
     &          istack_export, item_export)
!
      call update_ele_by_double_address(ucd%nele, ucd%nnod_4_ele,       &
     &    ucd%ie)
!
!
      if (ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_init_hdf5
      end if
!
      end subroutine init_merged_ucd
!
!  ---------------------------------------------------------------------
!
      subroutine finalize_merged_ucd(ucd)
!
      use hdf5_file_IO
!
      type(ucd_data), intent(in) :: ucd
!
!
      if (ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_finalize_hdf5
      end if
!
      end subroutine finalize_merged_ucd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_merged_ucd_file(istep, ucd)
!
      use merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_ucd,       &
     &      istep, file_name)
!
        write(*,*) 'single UCD data: ', trim(file_name)
        open(id_vtk_file,file=file_name, form='formatted')
      end if
!
      call write_merged_ucd_mesh(id_vtk_file, ucd%nnod, ucd%nele,       &
     &    ucd%nnod_4_ele, ucd%xx, ucd%ie, ucd%ntot_comp)
!
      call write_merged_ucd_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_udt_file(istep, ucd)
!
      use merged_ucd_data_IO
!
      integer(kind = kint), intent(in) ::  istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_udt,       &
     &      istep, file_name)
!
        write(*,*) 'single UCD field data: ', trim(file_name)
        open(id_vtk_file,file=file_name, form='formatted')
      end if
!
      call write_merged_ucd_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_grd_file(ucd)
!
      use merged_ucd_data_IO
!
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd%file_prefix, iflag_udt,       &
     &      file_name)
!
        write(*,*) 'single UCD grid data: ', trim(file_name)
        open (id_vtk_file, file=file_name, status='replace')
      end if
!
      call write_merged_ucd_mesh(id_vtk_file, ucd%nnod, ucd%nele,       &
     &    ucd%nnod_4_ele, ucd%xx, ucd%ie, ucd%ntot_comp)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_merged_vtk_file(istep, ucd)
!
      use merged_vtk_data_IO
!
      integer(kind = kint), intent(in) ::  istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_vtk,       &
     &      istep, file_name)
!
        write(*,*) 'single VTK data: ', trim(file_name)
        open (id_vtk_file, file=file_name, form='formatted',            &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_mesh(id_vtk_file, ucd%nnod,                 &
     &    ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie)
!
      call write_merged_vtk_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_phys(istep, ucd)
!
      use merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_vtd,       &
     &      istep, file_name)
!
        write(*,*) 'single VTK field data: ', file_name
        open (id_vtk_file, file=file_name, form='formatted',            &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_grid(ucd)
!
      use merged_vtk_data_IO
!
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd%file_prefix, iflag_vtd,       &
     &      file_name)
!
        write(*,*) 'single VTK grid data:     ', trim(file_name)
        open (id_vtk_file,  file=file_name, form='formatted',           &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_mesh(id_vtk_file, ucd%nnod,                 &
     &    ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module merged_udt_vtk_file_IO
