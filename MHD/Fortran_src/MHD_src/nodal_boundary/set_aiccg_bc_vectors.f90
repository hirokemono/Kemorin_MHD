!set_aiccg_bc_vectors.f90
!      module set_aiccg_bc_vectors
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on Nov. 2003
!        modified by H. Matsui on Oct. 2005
!        modified by H. Matsui on Feb. 2009
!
!      subroutine set_aiccg_bc_phys
!
      module set_aiccg_bc_vectors
!
      use m_precision
!
      implicit none
!
      private :: set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_phys
!
      use calypso_mpi
      use m_control_parameter
!
      use set_aiccg_nod_bc_vect
      use set_aiccg_bc_scalars
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_aiccg_bc_press_nod
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call set_aiccg_bc_velo
        end if
      end if
!

      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call set_aiccg_bc_temp_nod
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call set_aiccg_bc_composition_nod
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call set_aiccg_bc_mag_p_nod
!
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call set_aiccg_bc_magne_nod
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_aiccg_bc_mag_p_nod
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call set_aiccg_bc_vecp_nod
        end if
      end if
!
!
      end subroutine set_aiccg_bc_phys
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo
!
      use m_bc_data_rotate
!
      use set_aiccg_free_sph
      use set_aiccg_nod_bc_vect
!
!
!      matrix setting for free slip on sphere
      call set_aiccg_bc_free_sphere
!
!      matrix setting for fixed boundaries
      call set_aiccg_bc_velo_nod
!
!
!        write(*,*) '  velo_bc_rotation'
      if ( num_index_ibc_vrot .ne. 0 ) then
       call set_aiccg_bc_velo_rot
      end if
!
      end subroutine set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_vectors
