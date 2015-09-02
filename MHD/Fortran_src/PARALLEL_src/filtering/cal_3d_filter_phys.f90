!
!      module cal_3d_filter_phys
!
!      Written by H. Matsui on Sep., 2007
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine cal_3d_filter_scalar_phys(num_filter_grp,             &
!     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!      subroutine cal_3d_filter_vector_phys(num_filter_grp,             &
!     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!      subroutine cal_3d_filter_tensor_phys(num_filter_grp,             &
!     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!         i_filter: field ID for filtered field
!         i_field:  field ID to be filtered
!         num_filter_grp:  num. of filtereing area
!         id_filter_grp:   table id for filtering
!
!      subroutine cal_3d_ez_filter_scalar_phys(num_filter_grp,          &
!     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!      subroutine cal_3d_ez_filter_vector_phys(num_filter_grp,          &
!     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!      subroutine cal_3d_ez_filter_tensor_phys(num_filter_grp,          &
!     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      module cal_3d_filter_phys
!
      use m_precision
!
      use m_array_for_send_recv
      use m_geometry_data
      use m_filter_coef_combained
      use m_nod_filter_comm_table
      use prepare_field_2_filter
      use send_recv_3d_filtering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_scalar_phys(num_filter_grp,              &
     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_filter_phys
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call prepare_scalar_2_filter(ntot_comp, i_field, d_nod)
!
      call sum_3d_filter_scalar_phys(num_filter_grp, id_filter_grp,     &
     &    ngrp_nod_3d_filter, istack_nod_3d_filter, ntot_nod_3d_filter, &
     &    inod_3d_filter, istack_near_nod_3d_filter,                    &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    istack_nsum_3d_filter, max_nsum_3d_filter,                    &
     &    ntot_nsum_3d_filter, ist_nsum_3d_filter, ied_nsum_3d_filter,  &
     &    nnod_filtering, x_vec_filtering(1), node1%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_3d_filter_scalar_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_vector_phys(num_filter_grp,              &
     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_filter_phys
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call prepare_vector_2_filter(ntot_comp, i_field, d_nod)
!
      call sum_3d_filter_vector_phys(num_filter_grp, id_filter_grp,     &
     &    ngrp_nod_3d_filter, istack_nod_3d_filter, ntot_nod_3d_filter, &
     &    inod_3d_filter, istack_near_nod_3d_filter,                    &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    istack_nsum_3d_filter, max_nsum_3d_filter,                    &
     &    ntot_nsum_3d_filter, ist_nsum_3d_filter, ied_nsum_3d_filter,  &
     &    nnod_filtering, x_vec_filtering(1), node1%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_3d_filter_vector_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_tensor_phys(num_filter_grp,              &
     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_filter_phys
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call prepare_sym_tensor_2_filter(ntot_comp, i_field, d_nod)
!
      call sum_3d_filter_tensor_phys(num_filter_grp, id_filter_grp,     &
     &    ngrp_nod_3d_filter, istack_nod_3d_filter, ntot_nod_3d_filter, &
     &    inod_3d_filter, istack_near_nod_3d_filter,                    &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    istack_nsum_3d_filter, max_nsum_3d_filter,                    &
     &    ntot_nsum_3d_filter, ist_nsum_3d_filter, ied_nsum_3d_filter,  &
     &    nnod_filtering, x_vec_filtering(1), node1%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_3d_filter_tensor_phys
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_scalar_phys(num_filter_grp,           &
     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_ez_filter_phys
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call prepare_scalar_2_filter(ntot_comp, i_field, d_nod)
!
      call sum_3d_ez_filter_scalar_phys(num_filter_grp, id_filter_grp,  &
     &    ngrp_nod_3d_filter, istack_nod_3d_filter, ntot_nod_3d_filter, &
     &    inod_3d_filter, istack_near_nod_3d_filter,                    &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    nnod_filtering, x_vec_filtering(1), node1%numnod, x_vec(1) )
!
      call scalar_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_3d_ez_filter_scalar_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_vector_phys(num_filter_grp,           &
     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_ez_filter_phys
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call prepare_vector_2_filter(ntot_comp, i_field, d_nod)
!
      call sum_3d_ez_filter_vector_phys(num_filter_grp, id_filter_grp,  &
     &    ngrp_nod_3d_filter, istack_nod_3d_filter, ntot_nod_3d_filter, &
     &    inod_3d_filter, istack_near_nod_3d_filter,                    &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    nnod_filtering, x_vec_filtering(1), node1%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_3d_ez_filter_vector_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_tensor_phys(num_filter_grp,           &
     &          id_filter_grp, i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_ez_filter_phys
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call prepare_sym_tensor_2_filter(ntot_comp, i_field, d_nod)
!
      call sum_3d_ez_filter_tensor_phys(num_filter_grp, id_filter_grp,  &
     &    ngrp_nod_3d_filter, istack_nod_3d_filter, ntot_nod_3d_filter, &
     &    inod_3d_filter, istack_near_nod_3d_filter,                    &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    nnod_filtering, x_vec_filtering(1), node1%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_3d_ez_filter_tensor_phys
!
! ----------------------------------------------------------------------
!
      end module cal_3d_filter_phys
