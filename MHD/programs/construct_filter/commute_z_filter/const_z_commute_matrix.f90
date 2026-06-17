!
!      module const_z_commute_matrix
!
!        programmed by H. Matsui on June, 2007
!
!!      subroutine s_const_commute_matrix(numnod, neib_z, zfilter_wk,   &
!!     &                                  delta_z, mat_crs)
!!        integer (kind= kint), intent(in) :: numnod
!!        type(neighbour_data_z), intent(in) :: neib_z
!!        type(z_filter_work), intent(in) :: zfilter_wk
!!        real(kind = kreal), intent(in) :: delta_z(numnod)
!!        type(CRS_matrix), intent(inout) :: mat_crs
!
      module const_z_commute_matrix
!
      use m_precision
      use m_constants
!
      use t_crs_matrix
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_const_commute_matrix(numnod, neib_z, zfilter_wk,     &
     &                                  delta_z, mat_crs)
!
      use m_commute_filter_z
      use m_matrix_4_z_commute
      use m_z_filter_values
!
      use t_neighbour_index_z
      use t_neighbour_data_z
!
      integer (kind= kint), intent(in) :: numnod
      type(z_filter_work), intent(in) :: zfilter_wk
      type(neighbour_data_z), intent(in) :: neib_z
      real(kind = kreal), intent(in) :: delta_z(numnod)
!
      type(CRS_matrix), intent(inout) :: mat_crs
!
      integer (kind = kint) :: kk, kfact, inod, i, k1, jj, k2, k21
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
         jj = neib_z%nneib_nod(inod,1) + 1
          mat_crs%B_crs(i) = f_mom(kk)
        end do
!
       else
        do inod = 1, numnod
         i = k2 + ncomp_mat*(inod-1)
         jj = neib_z%nneib_nod(inod,1) + 1
          mat_crs%B_crs(i) = delta_z(inod)**kfact * f_mom(kk)
        end do
!         i = k2
!         jj =  neib_z%nneib_nod(1,1) + 1
!         mat_crs%B_crs(i) = 2.0*delta_z(1) * f_mom(kk)
!         i = k2 + ncomp_mat*(numnod-1)
!         jj =  neib_z%nneib_nod(numnod,1) + 1
!         mat_crs%B_crs(i) = 2.0d0*delta_z(numnod) * f_mom(kk)
       end if
!
        do k1 = 1, ncomp_mat
          do inod = 1, numnod
            jj = zfilter_wk%ncomp_z_st(inod) + k1 - 1
            k21 = k2 + (k1-1)*ncomp_mat + (inod-1)*ncomp_mat*ncomp_mat
            mat_crs%D_crs(k21) = d_norm_nod(inod,jj,kfact)
          end do
        end do
      end do
!
!
      end subroutine s_const_commute_matrix
!
!-----------------------------------------------------------------------
!
      end module const_z_commute_matrix
