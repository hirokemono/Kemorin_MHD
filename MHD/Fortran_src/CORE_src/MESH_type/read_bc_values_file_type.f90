!read_bc_values_file_type.f90
!     module read_bc_values_file_type
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
!      subroutine read_boundary_values_file_t(my_rank, group)
!        type(mesh_groups), intent(in) ::   group
!        integer (kind=kint), intent(in) :: my_rank
!
      module read_bc_values_file_type
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
      subroutine read_boundary_values_file_t(my_rank, group)
!
      use t_mesh_data
      use boundary_field_file_IO
!
      type(mesh_groups), intent(in) ::   group
      integer (kind=kint), intent(in) :: my_rank
!
!
      call read_boundary_values_file(my_rank, group%nod_grp%num_grp,    &
     &    group%nod_grp%istack_grp, group%nod_grp%grp_name,             &
     &    group%surf_grp%num_grp, group%surf_grp%istack_grp,            &
     &    group%surf_grp%grp_name)
!
      end subroutine read_boundary_values_file_t
!
!-----------------------------------------------------------------------
!
      end module read_bc_values_file_type
