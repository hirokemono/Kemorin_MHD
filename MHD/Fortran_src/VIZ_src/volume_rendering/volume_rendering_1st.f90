!volume_rendering_1st.f90
!      module volume_rendering_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_pvr(nod_fld)
!      subroutine visualize_pvr(istep_pvr, nod_fld, jac_3d)
!
!      subroutine pvr_init_1st(nod_fld)
!      subroutine pvr_main_1st(istep_pvr, nod_fld, jac_3d)
!
      module volume_rendering_1st
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_pvr(nod_fld)
!
      use m_control_data_pvrs
      use volume_rendering
!
      type(phys_data), intent(in) :: nod_fld
!
!
      num_pvr = num_pvr_ctl
      if (num_pvr .gt. 0) call pvr_init_1st(nod_fld)
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr(istep_pvr, nod_fld, jac_3d)
!
      use volume_rendering
      use t_jacobian_3d
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      if (num_pvr.gt.0 .and. istep_pvr.gt.0) then
        call pvr_main_1st(istep_pvr, nod_fld, jac_3d)
      end if
!
      end subroutine visualize_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine pvr_init_1st(nod_fld)
!
      use m_geometry_data
      use m_group_data
!
      use volume_rendering
!
      type(phys_data), intent(in) :: nod_fld
!
!
      call pvr_init(node1, ele1, surf1, ele_grp1, nod_fld)
!
      end subroutine pvr_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main_1st(istep_pvr, nod_fld, jac_3d)
!
      use m_geometry_data
      use t_jacobian_3d
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      call pvr_main(istep_pvr, node1, ele1, surf1, jac_3d, nod_fld)
!
      end subroutine pvr_main_1st
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_1st
