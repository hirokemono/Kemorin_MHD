!
!      module construct_commute_matrix
!
      module construct_commute_matrix
!
!        programmed by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_const_commute_matrix
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_const_commute_matrix
!
      use m_geometry_parameter
      use m_commute_filter_z
      use m_crs_matrix
      use m_matrix_4_commutation
      use m_neibor_data_z
      use m_filter_values
      use m_int_edge_vart_width
!
!
      integer (kind = kint) :: kk, kfact, inod, i, k1, ii, jj, k2
      real(kind = kreal), parameter :: zero = 0.0d0, one = 1.0d0
!
!
!   components for normalization on node
!
!
      do kk = 1, ncomp_norm
       kfact = kcomp_norm(kk)
       k2 = kk + 2
!
       if (kfact.eq.0) then
        do inod = 1, numnod
         i = k2 + ncomp_mat*(inod-1)
         jj = nneib_nod(inod,1) + 1
          B_crs(i) = f_mom(kk)
        end do
!
       else
        do inod = 1, numnod
         i = k2 + ncomp_mat*(inod-1)
         jj = nneib_nod(inod,1) + 1
          B_crs(i) = delta_z(inod)**kfact * f_mom(kk)
        end do
!         i = k2
!         jj =  nneib_nod(1,1) + 1
!         B_crs(i) = 2.0*delta_z(1) * f_mom(kk)
!         i = k2 + ncomp_mat*(numnod-1)
!         jj =  nneib_nod(numnod,1) + 1
!         B_crs(i) = 2.0d0*delta_z(numnod) * f_mom(kk)
       end if
!
       do k1 = 1, ncomp_mat
        do inod = 1, numnod
         jj = ncomp_st(inod) + k1 - 1
         D_crs(k2,k1,inod) = d_norm_nod(inod,jj,kfact)
        end do
       end do
      end do
!
!
      end subroutine s_const_commute_matrix
!
!-----------------------------------------------------------------------
!
      end module construct_commute_matrix
