!
!      module cal_w_filter_phys_smp
!
!      Written by H. Matsui on Nov., 2008
!
!!      subroutine cal_w_filter_scalar_phys_smp                         &
!!     &         (nod_comm, node, num_filter_grp, id_filter_grp,        &
!!     &          i_field, ntot_comp, i_filter, d_nod)
!!      subroutine cal_w_filter_vector_phys_smp                         &
!!     &         (nod_comm, node, num_filter_grp, id_filter_grp,        &
!!     &          i_field, ntot_comp, i_filter, d_nod)
!!      subroutine cal_w_filter_tensor_phys_smp                         &
!!     &         (nod_comm, node, num_filter_grp, id_filter_grp,        &
!!     &          i_field, ntot_comp, i_filter, d_nod)
!!         i_filter: field ID for filtered field
!!         i_field:  field ID to be filtered
!!         num_filter_grp:  num. of filtereing area
!!         id_filter_grp:   table id for filtering
!!
!!      subroutine cal_w_ez_filter_scalar_smp                           &
!!     &         (nod_comm, node, num_filter_grp, id_filter_grp,        &
!!     &          i_field, ntot_comp, i_filter, d_nod)
!!      subroutine cal_w_ez_filter_vector_smp                           &
!!     &         (nod_comm, node, num_filter_grp, id_filter_grp,        &
!!     &          i_field, ntot_comp, i_filter, d_nod)
!!      subroutine cal_w_ez_filter_tensor_smp                           &
!!     &         (nod_comm, node, num_filter_grp, id_filter_grp,        &
!!     &          i_field, ntot_comp, i_filter, d_nod)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!
      module cal_w_filter_phys_smp
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
!
      use m_array_for_send_recv
      use m_3d_w_filter_coef_smp
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
      subroutine cal_w_filter_scalar_phys_smp                           &
     &         (nod_comm, node, num_filter_grp, id_filter_grp,          &
     &          i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      call prepare_scalar_2_w_fil(node, ntot_comp, i_field, d_nod)
!
      call sum_3d_filter_scalar_phys_smp(num_filter_grp, id_filter_grp, &
     &    ngrp_nod_3d_w_fil_smp, istack_nod_3d_w_fil_smp,               &
     &    ntot_nod_3d_w_fil_smp, inod_3d_w_fil_smp,                     &
     &    istack_near_nod_3d_wf_smp, ntot_near_nod_3d_w_fil_smp,        &
     &    inod_near_nod_3d_w_smp, filter_weight_3d_w_smp,               &
     &    max_nsum_3d_w_fil_smp, istack_nsum_3d_wf_smp,                 &
     &    ntot_nsum_3d_w_fil_smp, ist_nsum_3d_w_fil_smp,                &
     &    ied_nsum_3d_w_fil_smp, nnod_w_filtering, x_vec_w_fil(1),      &
     &    node%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    ntot_comp, i_filter, d_nod)
!
      end subroutine cal_w_filter_scalar_phys_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filter_vector_phys_smp                           &
     &         (nod_comm, node, num_filter_grp, id_filter_grp,          &
     &          i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      call prepare_vector_2_w_fil(node, ntot_comp, i_field, d_nod)
!
      call sum_3d_filter_vector_phys_smp(num_filter_grp, id_filter_grp, &
     &    ngrp_nod_3d_w_fil_smp, istack_nod_3d_w_fil_smp,               &
     &    ntot_nod_3d_w_fil_smp, inod_3d_w_fil_smp,                     &
     &    istack_near_nod_3d_wf_smp, ntot_near_nod_3d_w_fil_smp,        &
     &    inod_near_nod_3d_w_smp, filter_weight_3d_w_smp,               &
     &    max_nsum_3d_w_fil_smp, istack_nsum_3d_wf_smp,                 &
     &    ntot_nsum_3d_w_fil_smp, ist_nsum_3d_w_fil_smp,                &
     &    ied_nsum_3d_w_fil_smp, nnod_w_filtering, x_vec_w_fil(1),      &
     &    node%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    ntot_comp, i_filter, d_nod)
!
      end subroutine cal_w_filter_vector_phys_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filter_tensor_phys_smp                           &
     &         (nod_comm, node, num_filter_grp, id_filter_grp,          &
     &          i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      call prepare_sym_tensor_2_w_fil(node, ntot_comp, i_field, d_nod)
!
      call sum_3d_filter_tensor_phys_smp(num_filter_grp, id_filter_grp, &
     &    ngrp_nod_3d_w_fil_smp, istack_nod_3d_w_fil_smp,               &
     &    ntot_nod_3d_w_fil_smp, inod_3d_w_fil_smp,                     &
     &    istack_near_nod_3d_wf_smp, ntot_near_nod_3d_w_fil_smp,        &
     &    inod_near_nod_3d_w_smp, filter_weight_3d_w_smp,               &
     &    max_nsum_3d_w_fil_smp, istack_nsum_3d_wf_smp,                 &
     &    ntot_nsum_3d_w_fil_smp, ist_nsum_3d_w_fil_smp,                &
     &    ied_nsum_3d_w_fil_smp, nnod_w_filtering, x_vec_w_fil(1),      &
     &    node%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    ntot_comp, i_filter, d_nod)
!
      end subroutine cal_w_filter_tensor_phys_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_w_ez_filter_scalar_smp                             &
     &         (nod_comm, node, num_filter_grp, id_filter_grp,          &
     &          i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_ez_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      call prepare_scalar_2_w_fil(node, ntot_comp, i_field, d_nod)
!
      call sum_3d_ez_filter_scalar_smp(num_filter_grp, id_filter_grp,   &
     &    ngrp_nod_3d_w_fil_smp, istack_nod_3d_w_fil_smp,               &
     &    ntot_nod_3d_w_fil_smp, inod_3d_w_fil_smp,                     &
     &    istack_near_nod_3d_wf_smp, ntot_near_nod_3d_w_fil_smp,        &
     &    inod_near_nod_3d_w_smp, filter_weight_3d_w_smp,               &
     &    nnod_w_filtering, x_vec_w_fil(1), node%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    ntot_comp, i_filter, d_nod)
!
      end subroutine cal_w_ez_filter_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_ez_filter_vector_smp                             &
     &         (nod_comm, node, num_filter_grp, id_filter_grp,          &
     &          i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_ez_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      call prepare_vector_2_w_fil(node, ntot_comp, i_field, d_nod)
!
      call sum_3d_ez_filter_vector_smp(num_filter_grp, id_filter_grp,   &
     &    ngrp_nod_3d_w_fil_smp, istack_nod_3d_w_fil_smp,               &
     &    ntot_nod_3d_w_fil_smp, inod_3d_w_fil_smp,                     &
     &    istack_near_nod_3d_wf_smp, ntot_near_nod_3d_w_fil_smp,        &
     &    inod_near_nod_3d_w_smp, filter_weight_3d_w_smp,               &
     &    nnod_w_filtering, x_vec_w_fil(1), node%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    ntot_comp, i_filter, d_nod)
!
      end subroutine cal_w_ez_filter_vector_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_ez_filter_tensor_smp                             &
     &         (nod_comm, node, num_filter_grp, id_filter_grp,          &
     &          i_field, ntot_comp, i_filter, d_nod)
!
      use sum_3d_ez_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ntot_comp, i_field, i_filter
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      call prepare_sym_tensor_2_w_fil(node, ntot_comp, i_field, d_nod)
!
      call sum_3d_ez_filter_tensor_smp(num_filter_grp, id_filter_grp,   &
     &    ngrp_nod_3d_w_fil_smp, istack_nod_3d_w_fil_smp,               &
     &    ntot_nod_3d_w_fil_smp, inod_3d_w_fil_smp,                     &
     &    istack_near_nod_3d_wf_smp, ntot_near_nod_3d_w_fil_smp,        &
     &    inod_near_nod_3d_w_smp, filter_weight_3d_w_smp,               &
     &    nnod_w_filtering, x_vec_w_fil(1), node%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    ntot_comp, i_filter, d_nod)
!
      end subroutine cal_w_ez_filter_tensor_smp
!
! ----------------------------------------------------------------------
!
      end module cal_w_filter_phys_smp
