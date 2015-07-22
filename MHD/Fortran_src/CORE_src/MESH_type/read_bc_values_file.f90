!read_bc_values_file.f90
!     module read_bc_values_file
!
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!
!!    format for data file for boundary data
!!
!!      line   :  number of type of boundary
!!      line...:  name of group type (node or surface) and group name
!!      line   :  name of physical values
!!               ( temperature, heat_flux, 
!!                 velocity_x, velocity_y, velocity_z,
!!                 torque_x, torque_y, torque_z, normal_velocity
!!                 pressure,
!!                 magnetic_x, magne_y, magne_z,
!!                 magnetic_potential, infinity
!!                 dunny_scalar )
!!      line...:   values
!!
!!      subroutine s_read_bc_values_file(my_rank, nod_grp, sf_grp)
!!        type(group_data), intent(in) :: nod_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        integer (kind=kint), intent(in) :: my_rank
!
      module read_bc_values_file
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
      subroutine s_read_bc_values_file(my_rank, nod_grp, sf_grp)
!
      use t_group_data
      use boundary_field_file_IO
!
      type(group_data), intent(in) :: nod_grp
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind=kint), intent(in) :: my_rank
!
!
      call read_boundary_values_file(my_rank,                           &
     &    nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name)
!
      end subroutine s_read_bc_values_file
!
!-----------------------------------------------------------------------
!
      end module read_bc_values_file
