!t_FEM_phys_data.f90
!     module t_FEM_phys_data
!
!> @brief nodal field data for FEM
!
!     Written by H. Matsui
!
      module t_FEM_phys_data
!
      use m_precision
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_material_property
!
      implicit  none
!
!
!>      Base structure for FEM_MHD
      type FEM_phys_data
!>        label   for simulation
        character(len=kchara)   :: label_sim
!
        type(mesh_data) :: femmesh
        type(element_geometry) :: ele_mesh
!
!>        address for nodal fields
        type(phys_address) :: iphys_nod
!>        Structure for nodal field data
        type(phys_data) :: nod_fld
!
!>       address for element fields
        type(phys_address) :: iphys_ele
!>        Structure for field data on element
        type(phys_data) :: ele_fld
!
!>        Strucutre of coefficients for each element
        type(coefs_4_MHD_type) :: ak_MHD
      end type FEM_phys_data
!
      end module t_FEM_phys_data
