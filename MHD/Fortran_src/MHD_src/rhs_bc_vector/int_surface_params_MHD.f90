!int_surface_params_MHD.f90
!     module int_surface_params_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine int_surface_parameters
!
      module int_surface_params_MHD
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
      subroutine int_surface_parameters
!
      use m_machine_parameter
      use m_control_parameter
      use m_surface_group
      use m_surface_group_connect
      use m_finite_element_matrix
      use m_int_surface_data
!
      use position_of_each_surface
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
!      use check_surface_groups
      use check_finite_element_mat
!
!
      if (sf_grp1%num_grp .le. 0) return
!
      call allocate_int_surf_data
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_norm_node
!
!
      if (iflag_debug.eq.1)  write(*,*) 'position_2_each_surface'
      call position_2_each_surface
!
      if (iflag_debug.eq.1)  write(*,*) 'delta_x_2_each_surface'
      call delta_x_2_each_surface
!
!      call check_surface_param_smp('int_surface_parameters end',       &
!     &    my_rank)
!
      end subroutine int_surface_parameters
!
!-----------------------------------------------------------------------
!
      end module int_surface_params_MHD
