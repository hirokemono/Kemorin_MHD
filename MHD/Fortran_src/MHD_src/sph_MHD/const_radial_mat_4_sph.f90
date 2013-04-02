!const_radial_mat_4_sph.f90
!      module const_radial_mat_4_sph
!
!     Written by H. Matsui on Oct, 2009
!
!      subroutine s_const_radial_mat_4_sph
!
      module const_radial_mat_4_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_radial_matrices_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_radial_mat_4_sph
!
      use m_control_parameter
      use m_control_params_sph_MHD
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!      use const_r_mat_4_momentum_sph
!
!
      if (iflag_t_evo_4_velo .gt. 0) then
        call allocate_velo_mat_sph
!
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step
      end if
!
      if (iflag_t_evo_4_temp .gt. 0) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_temp_sph'
        call allocate_temp_mat_sph
        call const_radial_mat_4_temp_sph
        if(i_debug .gt. 0) call check_temp_matrices_sph(my_rank)
      end if
!
      if (iflag_t_evo_4_magne .gt. 0) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_magne_sph'
        call allocate_magne_mat_sph
        call const_radial_mat_4_magne_sph
        if(i_debug .gt. 0) call check_magne_matrices_sph(my_rank)
      end if
!
      if(iflag_t_evo_4_composit .gt. 0) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_composit_sph'
        call allocate_composit_mat_sph
        call const_radial_mat_4_composit_sph
        if(i_debug .gt. 0) call check_composit_matrix_sph(my_rank)
      end if
!
!      if(i_debug .gt. 0) close(50+my_rank)
!
      end subroutine s_const_radial_mat_4_sph
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph
