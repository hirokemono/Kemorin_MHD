!>@file   t_FEM_phys_data.f90
!!@brief  module t_FEM_phys_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Structure of field data for FEM
!!
!!@verbatim
!!      subroutine allocate_phys_data_type(mesh, FEM_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!         type(FEM_fields), intent(inout) :: FEM_fld
!!@endverbatim
!
      module t_FEM_phys_data
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit  none
! 
!
!>       Structure for FEM field data
      type FEM_fields
!
!>       simulation name label
        character(len=kchara)   :: label_sim
!>       Structure for nodal data
        type(phys_data) :: phys_nod
!>       Structure for elemental data
        type(phys_data) :: phys_ele
!>       Structure for surface data
        type(phys_data) :: phys_surf
!>       Structure for edge data
        type(phys_data) :: phys_edge
!
        type(phys_address) :: id_phys_nod
!>       Structure for elemental data
        type(phys_address) :: id_phys_ele
!>       Structure for surface data
        type(phys_address) :: id_phys_surf
!>       Structure for edge data
        type(phys_address) :: id_phys_edge
      end type FEM_fields
!
      private :: set_field_address_type
!
! -------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine allocate_phys_data_type(mesh, FEM_fld)
!
      use t_mesh_data
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
!      write(*,*) 'alloc_phys_data_type node'
      call alloc_phys_data_type(mesh%node%numnod, FEM_fld%phys_nod)
!      write(*,*) 'alloc_phys_data_type element'
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
      use set_field_address
!
      type(phys_data), intent(inout) :: fld
      type(phys_address), intent(inout) :: phys_id
!
!
      call set_field_addresses(ione, fld%num_phys,                      &
     &    fld%phys_name, fld%num_component, phys_id)
!
      end subroutine set_field_address_type
!
!  --------------------------------------------------------------------
!
      end module t_FEM_phys_data
