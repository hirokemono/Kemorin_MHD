!const_connect_2_n_filter.f90
!      module const_connect_2_n_filter
!
!      Written by H. Matsui
!
!      subroutine set_connect_2_n_filter
!
      module const_connect_2_n_filter
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
      subroutine set_connect_2_n_filter
!
      use m_geometry_data
      use m_commute_filter_z
      use m_neibor_data_z
!
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: i
!
!
      do inod = 1, node1%numnod
        i = node1%inod_global(inod)
        ncomp_st(inod) = max(1, 1+nneib_nod(i,1) - (ncomp_mat-1)/2 )
        ncomp_st(inod) = min(ncomp_st(inod)+ncomp_mat-1, nfilter2_3)    &
                  - ncomp_mat + 1
      end do
!
      end subroutine set_connect_2_n_filter
!
!   --------------------------------------------------------------------
!
      end module const_connect_2_n_filter
