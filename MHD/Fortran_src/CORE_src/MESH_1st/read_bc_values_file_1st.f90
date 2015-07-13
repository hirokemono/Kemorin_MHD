!read_bc_values_file_1st.f90
!     module read_bc_values_file_1st
!
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!
!    format for data file for boundary data
!
!      line   :  number of type of boundary
!      line...:  name of group type (node or surface) and group name
!      line   :  name of physical values
!               ( temperature, heat_flux, 
!                 velocity_x, velocity_y, velocity_z,
!                 torque_x, torque_y, torque_z, normal_velocity
!                 pressure,
!                 magnetic_x, magne_y, magne_z,
!                 magnetic_potential, infinity
!                 dunny_scalar )
!      line...:   values
!
!      subroutine read_boundary_values_file_1(my_rank)
!
      module read_bc_values_file_1st
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_boundary_values_file_1(my_rank)
!
      use m_node_group
      use m_surface_group
      use boundary_field_file_IO
!
      integer (kind=kint), intent(in) :: my_rank
!
!
      call read_boundary_values_file(my_rank,                           &
     &    nod_grp1%num_grp, nod_grp1%istack_grp, bc_name,               &
     &    sf_grp1%num_grp, sf_grp1%istack_grp, sf_grp1%grp_name)
!
      end subroutine read_boundary_values_file_1
!
!-----------------------------------------------------------------------
!
      end module read_bc_values_file_1st
