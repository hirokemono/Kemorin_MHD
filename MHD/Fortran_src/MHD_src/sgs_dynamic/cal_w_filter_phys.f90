!
!      module cal_w_filter_phys
!
!      Written by H. Matsui on Sep., 2007
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine cal_w_filter_scalar_phys(num_filter_grp,              &
!     &          id_filter_grp, i_filter, i_field)
!      subroutine cal_w_filter_vector_phys(num_filter_grp,              &
!     &          id_filter_grp, i_filter, i_field)
!      subroutine cal_w_filter_tensor_phys(num_filter_grp,              &
!     &          id_filter_grp, i_filter, i_field)
!         i_filter: field ID for filtered field
!         i_field:  field ID to be filtered
!         num_filter_grp:  num. of filtereing area
!         id_filter_grp:   table id for filtering
!
!      subroutine cal_w_ez_filter_scalar_phys(num_filter_grp,           &
!     &          id_filter_grp, i_filter, i_field)
!      subroutine cal_w_ez_filter_vector_phys(num_filter_grp,           &
!     &          id_filter_grp, i_filter, i_field)
!      subroutine cal_w_ez_filter_tensor_phys(num_filter_grp,           &
!     &          id_filter_grp, i_filter, i_field)
!
      module cal_w_filter_phys
!
      use m_precision
!
      use m_array_for_send_recv
      use m_geometry_data
      use m_node_phys_data
      use m_3d_w_filter_coef
      use m_nod_w_filter_comm_table
      use prepare_field_2_w_filter
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
      subroutine cal_w_filter_scalar_phys(num_filter_grp,               &
     &          id_filter_grp, i_filter, i_field)
!
      use sum_3d_filter_phys
!
      integer(kind = kint), intent(in) :: i_filter, i_field
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
!
      call prepare_scalar_2_w_fil(i_field)
!
      call sum_3d_filter_scalar_phys(num_filter_grp, id_filter_grp,     &
     &    ngrp_nod_3d_w_fil, istack_nod_3d_w_fil, ntot_nod_3d_w_fil,    &
     &    inod_3d_w_filter, istack_near_nod_3d_w_fil,                   &
     &    ntot_near_nod_3d_w_fil, inod_near_nod_3d_w,                   &
     &    filter_weight_3d_w, istack_nsum_3d_w_fil, max_nsum_3d_w_fil,  &
     &    ntot_nsum_3d_w_fil, ist_nsum_3d_w_fil, ied_nsum_3d_w_fil,     &
     &    nnod_w_filtering, x_vec_w_fil(1), node1%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_w_filter_scalar_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filter_vector_phys(num_filter_grp,               &
     &          id_filter_grp, i_filter, i_field)
!
      use sum_3d_filter_phys
!
      integer(kind = kint), intent(in) :: i_filter, i_field
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
!
      call prepare_vector_2_w_fil(i_field)
!
      call sum_3d_filter_vector_phys(num_filter_grp, id_filter_grp,     &
     &    ngrp_nod_3d_w_fil, istack_nod_3d_w_fil, ntot_nod_3d_w_fil,    &
     &    inod_3d_w_filter, istack_near_nod_3d_w_fil,                   &
     &    ntot_near_nod_3d_w_fil, inod_near_nod_3d_w,                   &
     &    filter_weight_3d_w, istack_nsum_3d_w_fil, max_nsum_3d_w_fil,  &
     &    ntot_nsum_3d_w_fil, ist_nsum_3d_w_fil, ied_nsum_3d_w_fil,     &
     &    nnod_w_filtering, x_vec_w_fil(1), node1%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_w_filter_vector_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filter_tensor_phys(num_filter_grp,               &
     &          id_filter_grp, i_filter, i_field)
!
      use sum_3d_filter_phys
!
      integer(kind = kint), intent(in) :: i_filter, i_field
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
!
      call prepare_sym_tensor_2_w_fil(i_field)
!
      call sum_3d_filter_tensor_phys(num_filter_grp, id_filter_grp,     &
     &    ngrp_nod_3d_w_fil, istack_nod_3d_w_fil, ntot_nod_3d_w_fil,    &
     &    inod_3d_w_filter, istack_near_nod_3d_w_fil,                   &
     &    ntot_near_nod_3d_w_fil, inod_near_nod_3d_w,                   &
     &    filter_weight_3d_w, istack_nsum_3d_w_fil, max_nsum_3d_w_fil,  &
     &    ntot_nsum_3d_w_fil, ist_nsum_3d_w_fil, ied_nsum_3d_w_fil,     &
     &    nnod_w_filtering, x_vec_w_fil(1), node1%numnod, x_vec(1) )
!
      call tensor_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_w_filter_tensor_phys
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_w_ez_filter_scalar_phys(num_filter_grp,            &
     &          id_filter_grp, i_filter, i_field)
!
      use sum_3d_ez_filter_phys
!
      integer(kind = kint), intent(in) :: i_filter, i_field
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
!
      call prepare_scalar_2_w_fil(i_field)
!
      call sum_3d_ez_filter_scalar_phys(num_filter_grp, id_filter_grp,  &
     &    ngrp_nod_3d_w_fil, istack_nod_3d_w_fil, ntot_nod_3d_w_fil,    &
     &    inod_3d_w_filter, istack_near_nod_3d_w_fil,                   &
     &    ntot_near_nod_3d_w_fil, inod_near_nod_3d_w,                   &
     &    filter_weight_3d_w, nnod_w_filtering, x_vec_w_fil(1),         &
     &    node1%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_w_ez_filter_scalar_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_ez_filter_vector_phys(num_filter_grp,            &
     &          id_filter_grp, i_filter, i_field)
!
      use sum_3d_ez_filter_phys
!
      integer(kind = kint), intent(in) :: i_filter, i_field
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
!
      call prepare_vector_2_w_fil(i_field)
!
      call sum_3d_ez_filter_vector_phys(num_filter_grp, id_filter_grp,  &
     &    ngrp_nod_3d_w_fil, istack_nod_3d_w_fil, ntot_nod_3d_w_fil,    &
     &    inod_3d_w_filter, istack_near_nod_3d_w_fil,                   &
     &    ntot_near_nod_3d_w_fil, inod_near_nod_3d_w,                   &
     &    filter_weight_3d_w, nnod_w_filtering, x_vec_w_fil(1),         &
     &    node1%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_w_ez_filter_vector_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_ez_filter_tensor_phys(num_filter_grp,            &
     &          id_filter_grp, i_filter, i_field)
!
      use sum_3d_ez_filter_phys
!
      integer(kind = kint), intent(in) :: i_filter, i_field
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
!
      call prepare_sym_tensor_2_w_fil(i_field)
!
      call sum_3d_ez_filter_tensor_phys(num_filter_grp, id_filter_grp,  &
     &    ngrp_nod_3d_w_fil, istack_nod_3d_w_fil, ntot_nod_3d_w_fil,    &
     &    inod_3d_w_filter, istack_near_nod_3d_w_fil,                   &
     &    ntot_near_nod_3d_w_fil, inod_near_nod_3d_w,                   &
     &    filter_weight_3d_w, nnod_w_filtering, x_vec_w_fil(1),         &
     &    node1%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(node1%numnod, x_vec(1),           &
     &    d_nod(1,i_filter))
!
      end subroutine cal_w_ez_filter_tensor_phys
!
! ----------------------------------------------------------------------
!
      end module cal_w_filter_phys
