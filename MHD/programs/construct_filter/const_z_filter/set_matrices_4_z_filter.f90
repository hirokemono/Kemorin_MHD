!set_matrices_4_z_filter.f90
!      module set_matrices_4_z_filter
!
!      Written by H. Matsui
!
!      subroutine set_consist_mass_mat
!      subroutine set_matrix_4_border
!
      module set_matrices_4_z_filter
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_consist_mass_mat
!
      use m_consist_mass_crs
      use m_geometry_parameter
      use m_int_edge_data
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, numnod
        d_mk_crs(inod) = mk_c(inod,inod)
      end do
      do inod = 2, numnod
        al_mk_crs(inod-1) = mk_c(inod-1,inod)
      end do
      do inod = 1, numnod-1
        au_mk_crs(inod) = mk_c(inod+1,inod)
      end do
!
      end subroutine set_consist_mass_mat
!
!   --------------------------------------------------------------------
!
      subroutine set_matrix_4_border
!
      use m_geometry_parameter
      use m_commute_filter_z
      use m_crs_matrix
      use m_matrix_4_z_commute
      use m_neibor_data_z
      use m_z_filter_values
!
      integer (kind = kint) :: kk, kfact, inod, i, k1, ii, jj, k2
      real(kind = kreal), parameter :: zero = 0.0d0, one = 1.0d0
!
!
!   components for normalization on node
!
      do inod = 1, numnod
        i = 1 + ncomp_mat*(inod-1)
        B_crs(i) = zero
        i = ncomp_mat*inod
        B_crs(i) = 2 + ncomp_mat*(inod-1)
      end do
      do inod = 1, numnod
        if (nneib_nod(inod,1) .lt. ((ncomp_mat-1)/2) ) then
          D_crs(1,ncomp_mat-1,inod) = one
        else
          D_crs(1,1,inod) = one
        end if
        if (nneib_nod(inod,2) .lt. ((ncomp_mat-1)/2) ) then
          D_crs(2,2,inod) = one
        else
          D_crs(2,ncomp_mat,inod) = one
        end if
      end do
!
      end subroutine set_matrix_4_border
!
!   --------------------------------------------------------------------
!
      end module set_matrices_4_z_filter
