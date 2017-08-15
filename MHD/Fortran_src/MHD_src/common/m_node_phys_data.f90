!m_node_phys_data.f90
!     module m_node_phys_data
!
!> @brief nodal field data for FEM
!
!     Written by H. Matsui
!
      module m_node_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
!
      implicit  none
!
!>       label   for simulation
      character(len=kchara)   :: label_sim
!
!>       Structure for nodal field data
      type(phys_data), save :: nod_fld1
!
!>       address for nodal fields
      type(phys_address), save :: iphys_nod1
!
      end module m_node_phys_data
