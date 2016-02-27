!>@file   FEM_analyzer_sph_MHD_w_viz.f90
!!@brief  module FEM_analyzer_sph_MHD_w_viz
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2014
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_w_viz
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  istep_psf    Time step increment for cross sectioning
!!@n @param  istep_iso    Time step increment for iso surfaces
!!@n @param  istep_pvr    Time step increment for volume rendering
!!@n @param  istep_fline  Time step increment for field line generation
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_MHD_w_viz
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_w_viz
!
      use m_mesh_data
      use m_geometry_data
      use m_group_data
      use m_t_step_parameter
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_element_id_4_node
!
      use set_ele_id_4_node_type
      use FEM_analyzer_sph_MHD
      use int_volume_of_domain
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  init FEM mesh data
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD
!
!  -------------------------------
!
      if( (i_step_output_fline) .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node                                          &
     &    (mesh1%node, mesh1%ele, next_tbl1%neib_ele)
      end if
!
      if(i_step_output_pvr .le. 0) Return
!
!  -----  If there is no volume rendering... return
!
      if (iflag_debug.eq.1) write(*,*)  'maximum_integration_points'
      call maximum_integration_points(ione)
      call const_jacobian_and_volume                                    &
     &   (mesh1%node, sf_grp1, infty_list, mesh1%ele,                   &
     &    jac1_3d_l, jac1_3d_q)
!
      end subroutine FEM_initialize_w_viz
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD_w_viz
