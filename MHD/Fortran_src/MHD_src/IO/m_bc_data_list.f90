!>@file   m_bc_data_list.f90
!!@brief  module m_bc_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief  Boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!@endverbatim
!
      module m_bc_data_list
!
      use m_precision
      use t_bc_data_list
!
      implicit  none
!
!
!>       Node group data list for velocity
      type(boundary_condition_list), save :: velo_nod
!>       Node group data list for pressure
      type(boundary_condition_list), save :: press_nod
!
!>       Node group data list for temperarure
      type(boundary_condition_list), save :: temp_nod
!>       Node group data list for composition
      type(boundary_condition_list), save :: light_nod
!
!>       Node group data list for magnetic field
      type(boundary_condition_list), save :: magne_nod
!>       Node group data list for magnetic vector potential
      type(boundary_condition_list), save :: a_potential_nod
!>       Node group data list for electric scalar potential
      type(boundary_condition_list), save :: e_potential_nod
!>       Node group data list for current density
      type(boundary_condition_list), save :: current_nod
!
!
      end module m_bc_data_list
