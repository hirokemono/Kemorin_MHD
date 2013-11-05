!set_aiccg_bc_scalars.f90
!      module set_aiccg_bc_scalars
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        Merged by Kemorin on June. 2005
!        Merged by Kemorin on Oct. 2005
!
!      subroutine set_aiccg_bc_press_nod
!      subroutine set_aiccg_bc_temp_nod
!      subroutine set_aiccg_bc_composition_nod
!      subroutine set_aiccg_bc_mag_p_nod
!
      module set_aiccg_bc_scalars
!
      use m_precision
!
      use m_geometry_parameter
      use set_aiccg_boundaries
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_press_nod
!
      use m_geometry_constants
      use m_bc_data_press
!
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      if ( num_index_ibc2_press .gt. 0 ) then
!
        do k0 = 1, num_index_ibc2_press
!
          iele = ele_bc2_p_id(k0)
!
          k1 = nod_bc2_p_id(k0)
          do k2 = 1, num_t_linear
            call set_bc_4_press_mat(iele, k1, k2)
          end do
!
          k2 = nod_bc2_p_id(k0)
          do k1 = 1, num_t_linear
            call set_bc_4_press_mat(iele, k1, k2)
          end do
!
        end do
      end if
!
      end subroutine set_aiccg_bc_press_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_temp_nod
!
      use m_bc_data_ene
!
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      if ( num_index_ibc2_temp .ne. 0 ) then
       do k0 = 1, num_index_ibc2_temp
        iele = ele_bc2_temp_id(k0)
!
        k1 = nod_bc2_temp_id(k0)
        do k2 = 1, nnod_4_ele
          call set_bc_4_temp_mat(iele, k1, k2)
        end do
!
        k2 = nod_bc2_temp_id(k0)
        do k1 = 1, nnod_4_ele
          call set_bc_4_temp_mat(iele, k1, k2)
        end do
       end do
      end if
!
      end subroutine set_aiccg_bc_temp_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_composition_nod
!
      use m_bc_data_composition
!
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      if ( num_index_ibc2_compsition .ne. 0 )  then
!
        do k0 = 1, num_index_ibc2_compsition
          iele = ele_bc2_composit_id(k0)
!
          k1 = nod_bc2_composit_id(k0)
          do k2 = 1, nnod_4_ele
            call set_bc_4_composite_mat(iele, k1, k2)
          end do
!
          k2 = nod_bc2_composit_id(k0)
          do k1 = 1, nnod_4_ele
            call set_bc_4_composite_mat(iele, k1, k2)
          end do
!
        end do
      end if
!
      end subroutine set_aiccg_bc_composition_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_mag_p_nod
!
      use m_geometry_constants
      use m_bc_data_magne_p
      use m_solver_djds_MHD
      use m_magne_matrix
      use set_aiccg_bc_node_type
!
!
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      if ( num_index_ibc2_mag_p .gt. 0 ) then
!
        do k0 = 1, num_index_ibc2_mag_p
          iele = ele_bc2_mag_p_id(k0)
!
          k1 = nod_bc2_mag_p_id(k0)
          do k2 = 1, num_t_linear
            call set_bc_4_scalar_mat_type(ie(iele,k1), ie(iele,k2),     &
     &          DJDS_linear, Fmat_DJDS)
          end do
!
          k2 = nod_bc2_mag_p_id(k0)
          do k1 = 1, num_t_linear
            call set_bc_4_scalar_mat_type(ie(iele,k1), ie(iele,k2),     &
     &          DJDS_linear, Fmat_DJDS)
          enddo
!
        enddo
      end if
!
      end subroutine set_aiccg_bc_mag_p_nod
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_scalars
