!>@file   set_matrices_4_z_filter.f90
!!        module set_matrices_4_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Copy matrices for solver
!!
!!@verbatim
!!      subroutine set_consist_mass_mat(numnod, mk_mat)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(in) :: mk_mat(numnod,numnod)
!!      subroutine set_matrix_4_border(numnod, neib_z, mat_crs)
!!        integer (kind = kint), intent(in) :: numnod
!!        type(neighbour_data_z), intent(in) :: neib_z
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!@endverbatim
!
      module set_matrices_4_z_filter
!
      use m_precision
      use m_constants
!
      use t_crs_matrix
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_consist_mass_mat(numnod, mk_mat)
!
      use m_consist_mass_crs
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: mk_mat(numnod,numnod)
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, numnod
        d_mk_crs(inod) = mk_mat(inod,inod)
      end do
      do inod = 2, numnod
        al_mk_crs(inod-1) = mk_mat(inod-1,inod)
      end do
      do inod = 1, numnod-1
        au_mk_crs(inod) = mk_mat(inod+1,inod)
      end do
!
      end subroutine set_consist_mass_mat
!
!   --------------------------------------------------------------------
!
      subroutine set_matrix_4_border(numnod, neib_z, mat_crs)
!
      use t_neighbour_data_z
      use m_commute_filter_z
      use m_matrix_4_z_commute
!
      integer (kind = kint), intent(in) :: numnod
      type(neighbour_data_z), intent(in) :: neib_z
!
      type(CRS_matrix), intent(inout) :: mat_crs
      integer (kind = kint) :: inod, i, ji
!
!
!   components for normalization on node
!
      do inod = 1, numnod
        i = 1 + ncomp_mat*(inod-1)
        mat_crs%B_crs(i) = zero
        i = ncomp_mat*inod
        mat_crs%B_crs(i) = 2 + ncomp_mat*(inod-1)
      end do
      do inod = 1, numnod
        if (neib_z%nneib_nod(inod,1) .lt. ((ncomp_mat-1)/2) ) then
          ji = 1 + (ncomp_mat-2) * ncomp_mat                            &
     &           + (inod-1) * ncomp_mat*ncomp_mat
          mat_crs%D_crs(ji) = one
        else
          ji = 1 + (inod-1) * ncomp_mat*ncomp_mat
          mat_crs%D_crs(ji) = one
        end if
        if (neib_z%nneib_nod(inod,2) .lt. ((ncomp_mat-1)/2) ) then
          ji = 2 + (2-1) * ncomp_mat + (inod-1) * ncomp_mat*ncomp_mat
          mat_crs%D_crs(ji) = one
        else
          ji = 2 + (ncomp_mat-1) * ncomp_mat                            &
     &           + (inod-1) * ncomp_mat*ncomp_mat
          mat_crs%D_crs(ji) = one
        end if
      end do
!
      end subroutine set_matrix_4_border
!
!   --------------------------------------------------------------------
!
      end module set_matrices_4_z_filter
