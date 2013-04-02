!
!     module node_equaly_sectioning
!
!
!     Written by K. Nakajima
!     Modified by H. Matsui
!
!       output: IGROUP_nod
!
!      subroutine equaly_bisection(nnod, inter_nod, xx)
!      subroutine eb_spherical(nnod, inter_nod,                         &
!     &          radius, colatitude, longitude)
!
!      subroutine eb_spherical_w_egrp(nnod, inter_nod, num_mat,         &
!     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,       &
!     &          inod_ele_grp, radius, colatitude, longitude)
!
      module node_equaly_sectioning
!
      use m_precision
!
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
      use sort_by_position_4_rcb
      use sort_by_position_4_eb3d
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine equaly_bisection(nnod, inter_nod, xx)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call allocate_work_4_rcb(nnod)

      IGROUP_nod(1:nnod)= 0
      IGROUP_nod(1:inter_nod)= 1

      call s_sort_by_position_4_eb3d(inter_nod, ndivide_eb,             &
     &    IGROUP_nod(1), xx(1,1), xx(1,2), xx(1,3), VAL, IS1)
!
      call deallocate_work_4_rcb
!
      end subroutine equaly_bisection
!
!   --------------------------------------------------------------------
!
      subroutine eb_spherical(nnod, inter_nod,                          &
     &          radius, colatitude, longitude)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: colatitude(nnod)
      real(kind= kreal), intent(in) :: longitude(nnod)
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call allocate_work_4_rcb(nnod)

      IGROUP_nod(1:nnod)= 0
      IGROUP_nod(1:inter_nod)= 1

      call s_sort_by_position_4_eb3d(inter_nod, ndivide_eb,             &
     &    IGROUP_nod(1), radius(1), colatitude(1), longitude(1),        &
     &    VAL, IS1)
!
      call deallocate_work_4_rcb
!
      end subroutine eb_spherical
!
!   --------------------------------------------------------------------
!
      subroutine eb_spherical_w_egrp(nnod, inter_nod, num_mat,          &
     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,        &
     &          inod_ele_grp, radius, colatitude, longitude)
!
      integer(kind = kint), intent(in) :: nnod, inter_nod
!
      integer(kind = kint), intent(in) :: num_mat, ntot_node_ele_grp
      integer(kind = kint), intent(in) :: inod_stack_ele_grp(0:num_mat)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_ele_grp(ntot_node_ele_grp)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: colatitude(nnod)
      real(kind= kreal), intent(in) :: longitude(nnod)
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call allocate_work_4_rcb(nnod)

      IGROUP_nod(1:nnod)= 0
      IGROUP_nod(1:inter_nod)= 1

      call s_sort_by_position_w_grp(inter_nod, ndivide_eb, num_mat,     &
     &    mat_name, ntot_node_ele_grp, inod_stack_ele_grp,              &
     &    inod_ele_grp, num_egrp_layer, grp_layer_name, IGROUP_nod,     &
     &    radius(1), colatitude(1), longitude(1), VAL, IS1)
!
      call deallocate_work_4_rcb
!
      end subroutine eb_spherical_w_egrp
!
!   --------------------------------------------------------------------
!
      end module node_equaly_sectioning
