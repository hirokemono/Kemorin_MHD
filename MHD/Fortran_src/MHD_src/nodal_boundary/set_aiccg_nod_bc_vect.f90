!set_aiccg_nod_bc_vect.f90
!      module set_aiccg_nod_bc_vect
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        Merged by Kemorin on June. 2005
!
!      subroutine set_aiccg_bc_velo_nod
!      subroutine set_aiccg_bc_velo_rot
!      subroutine set_aiccg_bc_vecp_nod
!      subroutine set_aiccg_bc_magne_nod
!
      module set_aiccg_nod_bc_vect
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_phys_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
! ---------  set boundary array
!
      subroutine set_aiccg_bc_velo_nod
!
      use m_bc_data_velo
      use m_solver_djds_MHD
      use m_velo_matrix
      use set_aiccg_bc_node_type
!
      integer(kind = kint) :: nd
      integer(kind = kint) :: iele, k0, k1, k2
!
!
      do nd = 1, n_vector
        if ( num_idx_ibc2_v(nd) .ne. 0 ) then
!
          do k0 = 1, num_idx_ibc2_v(nd)
!
            iele = ele_bc2_v_id(k0,nd)
!
            k1 = nod_bc2_v_id(k0,nd)
            do k2 = 1, nnod_4_ele
              call set_bc_4_vector_mat_type(nd, nd,                     &
     &            ie(iele,k1), ie(iele,k2), DJDS_fluid, Vmat_DJDS)
            end do
!
            k2 = nod_bc2_v_id(k0,nd)
            do k1 = 1, nnod_4_ele
              call set_bc_4_vector_mat_type(nd, nd,                     &
     &            ie(iele,k1), ie(iele,k2), DJDS_fluid, Vmat_DJDS)
            end do
!
          end do
!
        end if
      end do
!
      end subroutine set_aiccg_bc_velo_nod
!
! -----------------------------------------------------------------------
! ---------  set boundary array for rotation
!
      subroutine set_aiccg_bc_velo_rot
!
      use m_bc_data_rotate
      use m_solver_djds_MHD
      use m_velo_matrix
      use set_aiccg_bc_node_type
!
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      do k0 = 1, num_index_ibc_vrot
        iele = ele_bc_vrot_id(k0)
!
        k1 = nod_bc_vrot_id(k0)
        do k2 = 1, nnod_4_ele
          call set_bc_4_vector_mat_type(ione, n_vector,                 &
     &            ie(iele,k1), ie(iele,k2), DJDS_fluid, Vmat_DJDS)
        end do
!
        k2 = nod_bc_vrot_id(k0)
        do k1 = 1, nnod_4_ele
          call set_bc_4_vector_mat_type(ione, n_vector,                 &
     &            ie(iele,k1), ie(iele,k2), DJDS_fluid, Vmat_DJDS)
        end do
      end do
!
      end subroutine set_aiccg_bc_velo_rot
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! ---------  set boundary array
!
      subroutine set_aiccg_bc_vecp_nod
!
      use m_bc_data_vect_p
      use m_solver_djds_MHD
      use m_magne_matrix
      use set_aiccg_bc_node_type
!
      integer(kind = kint) :: nd
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      do nd = 1, n_vector
        if ( num_idx_ibc2_vp(nd) .ne. 0 ) then
!
          do k0 = 1, num_idx_ibc2_vp(nd)
!
            iele = ele_bc2_vp_id(k0,nd)
!
            k1 = nod_bc2_vp_id(k0,nd)
            do k2 = 1, nnod_4_ele
              call set_bc_4_vector_mat_type(nd, nd,                     &
     &            ie(iele,k1), ie(iele,k2), DJDS_entire, Bmat_DJDS)
            end do
!
            k2 = nod_bc2_vp_id(k0,nd)
            do k1 = 1, nnod_4_ele
              call set_bc_4_vector_mat_type(nd, nd,                     &
     &            ie(iele,k1), ie(iele,k2), DJDS_entire, Bmat_DJDS)
            end do
!
          end do
!
        end if
      end do
!
      end subroutine set_aiccg_bc_vecp_nod
!
! -----------------------------------------------------------------------
! ---------  set boundary array
!
      subroutine set_aiccg_bc_magne_nod
!
      use m_bc_data_magne
      use m_solver_djds_MHD
      use m_magne_matrix
      use set_aiccg_bc_node_type
!
      integer(kind = kint) :: nd
      integer (kind = kint) :: iele, k0, k1, k2
!
!
      do nd = 1, n_vector
        if ( num_idx_ibc2_b(nd) .ne. 0 ) then
!
          do k0 = 1, num_idx_ibc2_b(nd)
!
            iele = ele_bc2_b_id(k0,nd)
!
            k1 = nod_bc2_b_id(k0,nd)
            do k2 = 1, nnod_4_ele
              call set_bc_4_vector_mat_type(nd, nd,                     &
     &            ie(iele,k1), ie(iele,k2), DJDS_entire, Bmat_DJDS)
            end do
!
            k2 = nod_bc2_b_id(k0,nd)
            do k1 = 1, nnod_4_ele
              call set_bc_4_vector_mat_type(nd, nd,                     &
     &            ie(iele,k1), ie(iele,k2), DJDS_entire, Bmat_DJDS)
            end do
!
          end do
!
        end if
      end do
!
      end subroutine set_aiccg_bc_magne_nod
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_nod_bc_vect
