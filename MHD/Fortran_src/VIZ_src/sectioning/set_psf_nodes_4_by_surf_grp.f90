!
!      module set_psf_nodes_4_by_surf_grp
!
!      Written by H. Matsui on June, 2006
!
!      subroutine count_node_at_node_on_grp(num_item, istack_n_on_n_smp)
!      subroutine set_node_at_node_on_grp(numnod, nitem_surf,           &
!     &          inod_surf_grp, nnod_on_nod,  istack_n_on_n_smp,        &
!     &          inod_4_nod, coef_on_nod, iflag_n_on_n, id_n_on_n)
!
!      subroutine count_node_on_edge_on_grp(istack_n_on_e_smp)
!
      module set_psf_nodes_4_by_surf_grp
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_at_node_on_grp(num_item, istack_n_on_n_smp)
!
      use cal_minmax_and_stacks
!
      integer(kind=kint), intent(in) :: num_item
      integer(kind=kint), intent(inout) :: istack_n_on_n_smp(0:np_smp)

      integer(kind=kint) :: ist, ied, max_4_smp
!
!
      ist = istack_n_on_n_smp(0)
      ied = istack_n_on_n_smp(0) + num_item
      call count_number_4_smp(np_smp, ist, ied,                         &
     &    istack_n_on_n_smp, max_4_smp)
!
      end subroutine count_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_on_grp(numnod, nitem_surf,            &
     &          inod_surf_grp, nnod_on_nod,  istack_n_on_n_smp,         &
     &          inod_4_nod, coef_on_nod, iflag_n_on_n, id_n_on_n)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nitem_surf
      integer(kind = kint), intent(in) :: inod_surf_grp(nitem_surf)
!
      integer(kind = kint), intent(in) :: nnod_on_nod
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: inod_4_nod(nnod_on_nod)
      integer(kind = kint), intent(inout) :: iflag_n_on_n(numnod)
      integer(kind = kint), intent(inout) :: id_n_on_n(numnod)
      real(kind= kreal), intent(inout) :: coef_on_nod(nnod_on_nod)
!
      integer(kind = kint) :: i, icou, inod
!
!
      do i = 1, nitem_surf
        icou = istack_n_on_n_smp(0) + i
        inod = inod_surf_grp(i)
        inod_4_nod(icou) = inod
        coef_on_nod(icou) = one
        iflag_n_on_n(inod) = ione
        id_n_on_n(inod) = icou
      end do
!
      end subroutine set_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_node_on_edge_on_grp(istack_n_on_e_smp)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_n_on_e_smp(0:np_smp)
!
      integer(kind = kint) :: i
!
!
      do i = 1, np_smp
        istack_n_on_e_smp(i) = istack_n_on_e_smp(0)
      end do
!
      end subroutine count_node_on_edge_on_grp
!
!  ---------------------------------------------------------------------
!
      end module set_psf_nodes_4_by_surf_grp
