!initialize_phys_data_type.f90
!------- module initialize_phys_data_type ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!       subroutine allocate_phys_data_type
!
!      subroutine set_element_filed_address
!
      module initialize_phys_data_type
!
      use m_precision
!
      implicit none
!
      private :: set_field_address_type
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_data_type(mesh, FEM_fld)
!
      use t_mesh_data
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_fields), intent(inout) :: FEM_fld
!
!    integer for work
!
      FEM_fld%label_sim = 'GeoFEM_MHD'
!
!  allocation for physical values
!
!      write(*,*) 'allocate_data_arrays', num_tot_nod_phys
      call alloc_phys_data_type(mesh%node%numnod, FEM_fld%phys_nod)
!      write(*,*) 'allocate_ele_data_arrays', num_tot_ele_phys
      call alloc_phys_data_type(mesh%ele%numele, FEM_fld%phys_ele)
!
!   set address of nodal values
!
!      write(*,*) 'set_field_address_type node'
      call set_field_address_type(FEM_fld%phys_nod, FEM_fld%id_phys_nod)
!
!   set address of elemental values
!
!      write(*,*) 'set_field_address_type element'
       call set_field_address_type(FEM_fld%phys_ele,                    &
     &     FEM_fld%id_phys_ele)
!
       end subroutine allocate_phys_data_type
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_field_address_type(fld, phys_id)
!
      use t_phys_address
      use t_phys_data
      use set_field_address
!
      type(phys_data), intent(inout) :: fld
      type(phys_address), intent(inout) :: phys_id
!
      call set_field_addresses(ione, fld%num_phys,                      &
     &    fld%phys_name, fld%num_component, phys_id)
!
      end subroutine set_field_address_type
!
!  --------------------------------------------------------------------
!
      end module initialize_phys_data_type
