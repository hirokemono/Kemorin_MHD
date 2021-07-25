!> @file  t_flags_each_comm_extend.f90
!!      module t_flags_each_comm_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
!!      subroutine alloc_flags_each_comm_extend(numnod, each_exp_flags)
!!      subroutine dealloc_flags_each_comm_extend(each_exp_flags)
!!      subroutine reset_flags_each_comm_extend(numnod, each_exp_flags)
!!        integer(kind = kint), intent(in) :: numnod, numele
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!
!!      subroutine init_min_dist_each_import                            &
!!     &         (igrp, sleeve_exp_p, nod_comm, node, ele, neib_ele,    &
!!     &          sleeve_exp_WK, dist_tmp)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        real(kind= kreal), intent(inout) :: dist_tmp(node%numnod)
!!      subroutine cal_min_dist_from_last_export(sleeve_exp_p,          &
!!     &         node, ele, neib_ele, num_each_export, item_each_export,&
!!     &         sleeve_exp_WK, each_exp_flags, iflag_exp_ele)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!        integer(kind = kint), intent(inout)                           &
!!     &                                   :: iflag_exp_ele(ele%numele)
!!
!!      subroutine set_new_export_to_extend(dist_max, node, distance,   &
!!     &          num_each_export, item_each_export, iflag_node)
!!        real(kind = kreal), intent(in) :: dist_max
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(in) :: distance(node%numnod)
!!        integer(kind = kint), intent(inout) :: num_each_export
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: item_each_export(node%numnod)
!!        integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!!@endverbatim
!
      module t_flags_each_comm_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_comm_table
      use t_comm_table_for_each_pe
!
      implicit none
!
      type flags_each_comm_extend
        integer(kind = kint), allocatable :: iflag_node(:)
        real(kind = kreal), allocatable :: distance(:)
      end type flags_each_comm_extend
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_flags_each_comm_extend(numnod, each_exp_flags)
!
      integer(kind = kint), intent(in) :: numnod
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
!
      allocate(each_exp_flags%iflag_node(numnod))
      allocate(each_exp_flags%distance(numnod))
!
      call reset_flags_each_comm_extend(numnod, each_exp_flags)
!
      end subroutine alloc_flags_each_comm_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_flags_each_comm_extend(each_exp_flags)
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
!
      deallocate(each_exp_flags%iflag_node)
      deallocate(each_exp_flags%distance)
!
      end subroutine dealloc_flags_each_comm_extend
!
!  ---------------------------------------------------------------------
!
      subroutine reset_flags_each_comm_extend(numnod, each_exp_flags)
!
      integer(kind = kint), intent(in) :: numnod
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
!
      if(numnod .gt. 0) then
!$omp parallel workshare
        each_exp_flags%iflag_node(1:numnod) = 0
        each_exp_flags%distance(1:numnod) =   0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine reset_flags_each_comm_extend
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_min_dist_each_import                              &
     &         (igrp, sleeve_exp_p, nod_comm, node, ele, neib_ele,      &
     &          sleeve_exp_WK, dist_tmp)
!
      use calypso_mpi
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
!
      integer(kind = kint), intent(in) :: igrp
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!
      real(kind= kreal), intent(inout) :: dist_tmp(node%numnod)
!
      integer(kind = kint) :: k1, jele
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jnod
      real(kind= kreal) :: dist, dist_start
!
!
      dist_tmp(1:node%numnod) = 0.0d0
!
      ist = nod_comm%istack_import(igrp-1) + 1
      ied = nod_comm%istack_import(igrp)
      do inum = ist, ied
        inod = nod_comm%item_import(inum)
        dist_tmp(inod) = -1.0d0
      end do
      do inum = ist, ied
        inod = nod_comm%item_import(inum)
        if(dist_tmp(inod) .eq. -1.0d0) then
          dist_start = 0.0d0
        else
          dist_start = dist_tmp(inod)
        end if
!
        jst = neib_ele%istack_4_node(inod-1) + 1
        jed = neib_ele%istack_4_node(inod)
        do jnum = jst, jed
          jele = neib_ele%iele_4_node(jnum)
          do k1 = 1, ele%nnod_4_ele
            jnod = ele%ie(jele,k1)
            if(dist_tmp(jnod) .eq. -1.0d0) cycle
!
            dist = distance_select(sleeve_exp_p, inod, jnod,            &
     &                             node, sleeve_exp_WK)
            if(dist_tmp(jnod) .eq. 0.0d0) then
              dist_tmp(jnod) = dist + dist_start
            else
              dist_tmp(jnod) = min(dist+dist_start, dist_tmp(jnod))
            end if
          end do
        end do
      end do
!
      end subroutine init_min_dist_each_import
!
!  ---------------------------------------------------------------------
!
      subroutine cal_min_dist_from_last_export(sleeve_exp_p,            &
     &         node, ele, neib_ele, num_each_export, item_each_export,  &
     &         sleeve_exp_WK, each_exp_flags, iflag_exp_ele)
!
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
      integer(kind = kint), intent(in) :: num_each_export
      integer(kind = kint), intent(in)                                  &
     &                     :: item_each_export(num_each_export)
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
      integer(kind = kint), intent(inout) :: iflag_exp_ele(ele%numele)
!
      integer(kind = kint) :: inum, inod, iflag
      integer(kind = kint) :: jst, jed, jnum, jele, jnod, k1
      real(kind = kreal) :: dist
!
!
      do inum = 1, num_each_export
        inod = item_each_export(inum)
        if(each_exp_flags%distance(inod)                                &
     &        .gt. sleeve_exp_p%dist_max) cycle
!
        jst = neib_ele%istack_4_node(inod-1) + 1
        jed = neib_ele%istack_4_node(inod)
        do jnum = jst, jed
          jele = neib_ele%iele_4_node(jnum)
          if(iflag_exp_ele(jele) .gt. 0) cycle
!
          iflag_exp_ele(jele) = 1
          do k1 = 1, ele%nnod_4_ele
            jnod = ele%ie(jele,k1)
            if(each_exp_flags%iflag_node(jnod) .lt. 0) cycle
!
            dist = distance_select(sleeve_exp_p, inod, jnod,            &
     &                             node, sleeve_exp_WK)
            if(each_exp_flags%iflag_node(jnod) .eq. 0) then
              each_exp_flags%iflag_node(jnod) = 1
              each_exp_flags%distance(jnod)                             &
     &              = dist + each_exp_flags%distance(inod)
            else
              each_exp_flags%distance(jnod)                             &
     &              = min(dist+each_exp_flags%distance(inod),           &
     &                    each_exp_flags%distance(jnod))
            end if
          end do
        end do
      end do
!
      inum = 0
      do jele = 1, ele%numele
        if(iflag_exp_ele(jele) .gt. 0) cycle
!
        iflag = 1
        do k1 = 1, ele%nnod_4_ele
          jnod = ele%ie(jele,k1)
          if(each_exp_flags%iflag_node(jnod) .eq. 0) then
            iflag = 0
            exit
          end if
        end do
        iflag_exp_ele(jele) = iflag
        inum = inum + iflag
      end do
!      write(*,*) 'Missing filled element: ', inum
!
      end subroutine cal_min_dist_from_last_export
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_export_to_extend(dist_max, node, distance,     &
     &          num_each_export, item_each_export, iflag_node)
!
      real(kind = kreal), intent(in) :: dist_max
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: distance(node%numnod)
!
      integer(kind = kint), intent(inout) :: num_each_export
      integer(kind = kint), intent(inout)                               &
     &                     :: item_each_export(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: jcou, inod
!
!
      jcou = 0
      do inod = 1, node%numnod
        if(iflag_node(inod) .gt. 0) then
          if(distance(inod) .lt. dist_max) then
            jcou = jcou + 1
            item_each_export(jcou) = inod
          end if
          iflag_node(inod) = -1
        end if
      end do
      num_each_export = jcou
!
      end subroutine set_new_export_to_extend
!
!  ---------------------------------------------------------------------
!
      end module t_flags_each_comm_extend
