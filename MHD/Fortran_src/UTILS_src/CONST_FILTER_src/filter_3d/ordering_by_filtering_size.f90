!
!      module ordering_by_filtering_size
!
!      Written by H. Matsui on Oct., 2006
!      Modified by H. Matsui on Mar., 2008
!
!!      subroutine filter_ordering_by_distance(node, inod, fil_coef)
!!      subroutine filter_ordering_by_dist_ratio                        &
!!     &         (node, FEM_elen, inod, fil_coef)
!!        type(node_data), intent(in) :: node
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!
!!      subroutine allocate_dist_ratio(numnod)
!
      module ordering_by_filtering_size
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dist_ratio(:)
      private :: dist_ratio
      private :: cal_distance_ratio_2_filter
      private :: cal_distance_from_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine filter_ordering_by_distance(node, inod, fil_coef)
!
      use t_geometry_data
      use t_filter_coefs
      use add_nodes_elems_4_each_nod
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      call cal_distance_from_filter                                     &
     &   (node%numnod, node%xx, inod, fil_coef)
!
      call sort_added_nod_4_each_by_real(node%numnod,                   &
     &    fil_coef%nnod_4_1nod_f, fil_coef%nnod_4_1nod_w,               &
     &    fil_coef%inod_4_1nod_w, dist_ratio)
!
      end subroutine filter_ordering_by_distance
!
! ----------------------------------------------------------------------
!
      subroutine filter_ordering_by_dist_ratio                          &
     &         (node, FEM_elen, inod, fil_coef)
!
      use t_geometry_data
      use t_filter_elength
      use t_filter_coefs
      use add_nodes_elems_4_each_nod
!
      type(node_data), intent(in) :: node
      type(gradient_model_data_type), intent(in) :: FEM_elen
      integer(kind = kint), intent(in) :: inod
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      call cal_distance_ratio_2_filter(node%numnod, node%xx, inod,      &
     &   FEM_elen%elen_nod%moms%f_x2(inod),                             &
     &   FEM_elen%elen_nod%moms%f_y2(inod),                             &
     &   FEM_elen%elen_nod%moms%f_z2(inod), fil_coef)
!
      call sort_added_nod_4_each_by_real(node%numnod,                   &
     &    fil_coef%nnod_4_1nod_f, fil_coef%nnod_4_1nod_w,               &
     &    fil_coef%inod_4_1nod_w, dist_ratio)
!
      end subroutine filter_ordering_by_dist_ratio
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_dist_ratio(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( dist_ratio(numnod) )
      dist_ratio = 0.0d0
!
      end subroutine allocate_dist_ratio
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_dist_ratio
!
      deallocate( dist_ratio )
!
      end subroutine deallocate_dist_ratio
!
! ----------------------------------------------------------------------
!
      subroutine cal_distance_ratio_2_filter(numnod, xx, inod,          &
     &          elen_dx2_nod, elen_dy2_nod, elen_dz2_nod, fil_coef)
!
      use t_filter_coefs
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: inod
      real(kind=kreal), intent(in) :: elen_dx2_nod
      real(kind=kreal), intent(in) :: elen_dy2_nod
      real(kind=kreal), intent(in) :: elen_dz2_nod
!
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint) :: inum, jnod
!
!
      do inum = 1, fil_coef%nnod_4_1nod_w
        jnod = fil_coef%inod_4_1nod_w(inum)
        dist_ratio(inum)                                                &
     &        = ((xx(jnod,1) - xx(inod,1))**2 / elen_dx2_nod)           &
     &        + ((xx(jnod,2) - xx(inod,2))**2 / elen_dy2_nod)           &
     &        + ((xx(jnod,3) - xx(inod,3))**2 / elen_dz2_nod)
      end do
!
      end subroutine cal_distance_ratio_2_filter
!
! ----------------------------------------------------------------------
!
      subroutine cal_distance_from_filter(numnod, xx, inod, fil_coef)
!
      use t_filter_coefs
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint) :: inum, jnod
!
!
      do inum = 1, fil_coef%nnod_4_1nod_w
        jnod = fil_coef%inod_4_1nod_w(inum)
        dist_ratio(inum) = ( (xx(jnod,1) - xx(inod,1))  )**2            &
     &                   + ( (xx(jnod,2) - xx(inod,2))  )**2            &
     &                   + ( (xx(jnod,3) - xx(inod,3))  )**2
      end do
!
      end subroutine cal_distance_from_filter
!
! ----------------------------------------------------------------------
!
      end module ordering_by_filtering_size
