!init_iccg_matrices.f90
!      module init_iccg_matrices
!
!        programmed by H.Matsui
!                                    on June 2005
!
!      subroutine allocate_aiccg_matrices
!      subroutine reset_aiccg_matrices
!
      module init_iccg_matrices
!
      use m_precision
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_magne_matrix
      use m_mag_potential_matrix
      use m_velo_matrix
      use m_press_matrix
      use m_temp_matrix
      use m_light_element_matrix
!
      implicit none
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_matrices
!
!
      if ( iflag_t_evo_4_velo .ge. 1 ) then
       call allocate_aiccg_press
!
       if ( iflag_t_evo_4_velo .ge. 3 ) then
        call allocate_aiccg_velo
       end if
      end if
!
      if ( iflag_t_evo_4_temp.ge.3 ) then
        call allocate_aiccg_temp
      end if
!
      if ( iflag_t_evo_4_composit.ge.3 ) then
       call allocate_aiccg_composit
      end if
!
      if ( iflag_t_evo_4_magne.ge.1 ) then
       call allocate_aiccg_mag_p
!       call allocate_aiccg_mag_p_ins
!       call allocate_aiccg_mag_p_cd
       if ( iflag_t_evo_4_magne .ge. 3 ) then
        call allocate_aiccg_magne
       end if
      end if
!
      if ( iflag_t_evo_4_vect_p.ge.1 ) then
        call allocate_aiccg_mag_p
!        call allocate_aiccg_mag_p_ins
!        call allocate_aiccg_mag_p_cd
       if ( iflag_t_evo_4_vect_p .ge. 3 ) then
        call allocate_aiccg_magne
       end if
      end if
!
      end subroutine allocate_aiccg_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine reset_aiccg_matrices
!
!
      if ( iflag_t_evo_4_velo .ge. 1 ) then
       call reset_aiccg_press
!
       if ( iflag_t_evo_4_velo .ge. 3 ) then
        call reset_aiccg_velo
       end if
      end if
!
      if ( iflag_t_evo_4_temp.ge.3 ) then
        call reset_aiccg_temp
      end if
!
      if ( iflag_t_evo_4_composit.ge.3 ) then
        call reset_aiccg_composit
      end if
!
      if ( iflag_t_evo_4_magne.ge.1 ) then
        call reset_aiccg_mag_p
!        call reset_aiccg_mag_p_ins
!        call reset_aiccg_mag_p_cd
        if ( iflag_t_evo_4_magne .ge. 3 ) then
          call reset_aiccg_magne
        end if
      end if
!
      if ( iflag_t_evo_4_vect_p.ge.1 ) then
        call reset_aiccg_mag_p
!        call reset_aiccg_mag_p_ins
!        call reset_aiccg_mag_p_cd
        if ( iflag_t_evo_4_vect_p .ge. 3 ) then
          call reset_aiccg_magne
        end if
      end if
!
      end subroutine reset_aiccg_matrices
!
!-----------------------------------------------------------------------
!
      end module init_iccg_matrices
