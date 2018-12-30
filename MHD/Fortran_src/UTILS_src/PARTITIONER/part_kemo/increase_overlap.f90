!
!     module increase_overlap
!
!     written by H. Matsui on Sep., 2007
!
!!      subroutine increase_overlapping(Ndomain, numnod, ele,           &
!!     &           n_overlap, i_sleeve_ele, included_ele)
!
      module increase_overlap
!
      use m_precision
      use m_constants
!
      use t_near_mesh_id_4_node
!
      implicit  none
!
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele_tmp
!
      integer(kind= kint) :: nele_subdomain
      integer(kind= kint), allocatable :: iflag_nod(:), iflag_ele(:)
      integer(kind= kint), allocatable :: item_tmp_e(:)
      integer(kind= kint), allocatable :: NPC_tmp2(:)
!
      private :: nele_subdomain, iflag_nod, iflag_ele
      private :: item_tmp_e, NPC_tmp2
!
      private :: mark_extented_overlap
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine increase_overlapping(Ndomain, node, ele, surf,        &
     &           field, iflag_selective, included_ele)
!
      use t_geometry_data
      use t_surface_data
      use intelligent_partition
      use m_domain_group_4_partition
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(node_data), intent(in) :: node
      type(vector_field), intent(in) :: field
      integer(kind = kint), intent(in) :: Ndomain
      integer(kind= kint), intent(in) :: iflag_selective
!      integer(kind = kint), intent(in) :: n_overlap, i_sleeve_ele
      type(near_mesh), intent(inout) :: included_ele
!
      integer(kind= kint) :: ip, inum, icel
!
!
      included_ele%ntot = included_ele%istack_nod(Ndomain)
      near_ele_tmp%ntot = included_ele%ntot
      call alloc_num_4_near_nod(Ndomain, near_ele_tmp)
      call alloc_near_element(near_ele_tmp)
!
      allocate( iflag_nod(node%numnod) )
      allocate( iflag_ele(ele%numele) )
      allocate( item_tmp_e(ele%numele) )
!
      iflag_nod = 0
      iflag_ele = 0
      item_tmp_e = 0
      do ip= 1, Ndomain
! extend overlap one by layer, new extend overlap layer is stored in near_ele_tmp
      if(iflag_selective .eq. 0) then
        call mark_extented_overlap                                      &
     &     (ip, n_overlap, i_sleeve_ele, node%numnod,                   &
     &      ele%numele, ele%nnod_4_ele, ele%ie, ele%nodelm,             &
     &      included_ele%ntot, included_ele%istack_nod,                 &
     &      included_ele%id_near_nod, nod_d_grp1%num_s_domin,           &
     &      nod_d_grp1%IGROUP)
      else
        call selective_extended_overlap(ip, node, ele, surf, field,     &
    &       included_ele%ntot, included_ele%istack_nod,                 &
    &       included_ele%id_near_nod)
      end if
!
        allocate(NPC_tmp2(near_ele_tmp%istack_nod(ip-1)) )
!
        near_ele_tmp%istack_nod(ip) = near_ele_tmp%istack_nod(ip-1)     &
     &                                 + nele_subdomain
        do icel = 1, near_ele_tmp%istack_nod(ip-1)
          NPC_tmp2(icel) = near_ele_tmp%id_near_nod(icel)
        end do
!
        near_ele_tmp%ntot = near_ele_tmp%istack_nod(ip)
        call dealloc_near_node(near_ele_tmp)
        call alloc_near_element(near_ele_tmp)
!
        do icel = 1, near_ele_tmp%istack_nod(ip-1)
          near_ele_tmp%id_near_nod(icel) = NPC_tmp2(icel)
        end do
        do icel = 1, nele_subdomain
          inum = near_ele_tmp%istack_nod(ip-1) + icel
          near_ele_tmp%id_near_nod(inum) = item_tmp_e(icel)
        end do
        near_ele_tmp%ntot = near_ele_tmp%istack_nod(Ndomain)
!
        write(*,*) 'ip, nele_subdomain',                                &
     &             ip, nele_subdomain, near_ele_tmp%istack_nod(ip)
!
        deallocate( NPC_tmp2 )
!
      end do
!
!    copy from work array
!
      included_ele%nmax = 0
      included_ele%nmin = near_ele_tmp%ntot
      do ip= 1, Ndomain
        included_ele%istack_nod(ip) = near_ele_tmp%istack_nod(ip)
        included_ele%num_nod(ip) = included_ele%istack_nod(ip)          &
     &                             - included_ele%istack_nod(ip-1)
        included_ele%nmax                                               &
     &             = max(included_ele%nmax,included_ele%num_nod(ip))
        included_ele%nmin                                               &
     &             = min(included_ele%nmin,included_ele%num_nod(ip))
      end do
      included_ele%ntot = near_ele_tmp%ntot
!
      call dealloc_near_node(included_ele)
      call alloc_near_element(included_ele)
!
      do inum = 1, included_ele%istack_nod(Ndomain)
        included_ele%id_near_nod(inum)                                  &
     &              = near_ele_tmp%id_near_nod(inum)
      end do
!
      deallocate( iflag_nod )
      deallocate( iflag_ele )
      deallocate( item_tmp_e )
      call dealloc_near_node(near_ele_tmp)
      call dealloc_num_4_near_node(near_ele_tmp)
!
      end subroutine increase_overlapping
!
!   --------------------------------------------------------------------
!
      subroutine mark_extented_overlap(ip, n_overlap, i_sleeve_ele,     &
     &          numnod, numele, nnod_4_ele, ie, nodelm,                 &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,  &
     &          nnod_s_domin, IGROUP_nod)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint), intent(in) :: n_overlap, i_sleeve_ele
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: nodelm(numele)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &              :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint), intent(in) :: nnod_s_domin
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_s_domin)
!
      integer(kind= kint) :: iwidth, inum, icel, inod, k, icou
      integer(kind= kint) :: ist, ied
!
!
        icou = 0
        item_tmp_e = 0
        ist = iele_stack_near_nod(ip-1) + 1
        ied = iele_stack_near_nod(ip)
        do inum = ist, ied
          icou = icou + 1
          item_tmp_e(icou) = iele_near_nod(inum)
        end do
        nele_subdomain = icou
!
        do iwidth = 2, n_overlap
!
!   Mark belonged node include internal and external
!
          iflag_nod = 0
          do inum = 1, nele_subdomain
            icel = item_tmp_e(inum)
!
            if (iwidth.eq.2 .and. i_sleeve_ele.eq.1) then
!
              inod = ie(icel,1)
              if (IGROUP_nod(inod) .eq. ip) then
                do k =1, nodelm(icel)
                  inod = ie(icel,k)
                  iflag_nod(inod) = 1
                end do
              end if
!
            else
              do k =1, nodelm(icel)
                inod = ie(icel,k)
                iflag_nod(inod) = 1
              end do
            end if
!
          end do
!
!    Mark number of belonged element
!
          iflag_ele = 0
          do icel= 1, numele
            do k =1, nodelm(icel)
              inod = ie(icel,k)
              if ( iflag_nod(inod) .eq. 1) then
                iflag_ele(icel) = 1
              end if
            end do
          end do
!
          icou = 0
          item_tmp_e = 0
          do icel= 1, numele
            if ( iflag_ele(icel) .eq. 1) then
              icou = icou + 1
              item_tmp_e(icou) = icel
            end if
          end do
          nele_subdomain = icou
        end do
!
      end subroutine mark_extented_overlap
!
!   --------------------------------------------------------------------
!
      subroutine selective_extended_overlap(ip, node, ele, surf, field,    &
      &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod)
!
      use t_geometry_data
      use t_surface_data
      use intelligent_partition
!
      integer(kind = kint), intent(in) :: ip
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(node_data), intent(in) :: node
      type(vector_field), intent(in) :: field
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in)                                  &
      &              :: iele_stack_near_nod(0:node%numnod)
      integer(kind = kint), intent(in)                                  &
      &              :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint) :: iwidth, inum, icel, inod, k, icou, i, iele
      integer(kind = kint) :: ist, ied, isf_tgt, isurf_end
      integer(kind = kint), allocatable :: iflag_ele_tmp(:)
      real(kind = kreal) :: x_start(3), v_start(3), x_tgt(3), xi(2)
!
!      write(*,*) 'selective extend overlap:', 'n_overlap:', n_overlap
      icou = 0
      item_tmp_e = 0
      iflag_ele = 0
      ist = iele_stack_near_nod(ip-1) + 1
      ied = iele_stack_near_nod(ip)
      do inum = ist, ied
        icou = icou + 1
        item_tmp_e(icou) = iele_near_nod(inum)
        iflag_ele(item_tmp_e(icou)) = 1
      end do
      nele_subdomain = icou
      allocate(iflag_ele_tmp(ele%numele))
!
      do iwidth = 2, n_overlap
        iflag_ele_tmp = 0
        do icel = 1, ele%numele
          if(iflag_ele(icel) .ne. 0) then
            iflag_ele_tmp(icel) = iflag_ele(icel)
      ! start a vector from center of current element, get the hitting surface
      ! the element on the other side of the surface will be the extended element
            x_start(1:3) = ele%x_ele(icel,1:3)
      ! interpolate center field data
            v_start(1:3) = 0.0
            do k = 1, ele%nodelm(icel)
              inod = ele%ie(icel,k)
              v_start(1:3) = v_start(1:3) + field%d_ucd(inod,1:3)
            end do
            v_start(1:3) = v_start(1:3)/k
            if(iwidth .eq. 2 .and. iflag_ele(icel) .eq. 1) then
              ! forward
              call find_line_end_in_1ele(1, node%numnod, ele%numele, surf%numsurf,        &
              &      surf%nnod_4_surf, surf%isf_4_ele, surf%ie_surf, node%xx, icel, 0,    &
              &      v_start, x_start, isf_tgt, x_tgt, xi)
              !
              if(isf_tgt .ne. 0) then
                isurf_end = abs(surf%isf_4_ele(icel,isf_tgt))
                do i = 1, 2
                  iele = surf%iele_4_surf(isurf_end,i,1)
                  if(iele .ne. icel .and. iele .ne. 0) iflag_ele_tmp(iele) = 2
                end do
              end if
              ! backward
              call find_line_end_in_1ele(-1, node%numnod, ele%numele, surf%numsurf,        &
              &      surf%nnod_4_surf, surf%isf_4_ele, surf%ie_surf, node%xx, icel, 0,    &
              &      v_start, x_start, isf_tgt, x_tgt, xi)
              !
              if(isf_tgt .ne. 0) then
                isurf_end = abs(surf%isf_4_ele(icel,isf_tgt))
                do i = 1, 2
                  iele = surf%iele_4_surf(isurf_end,i,1)
                  if(iele .ne. icel .and. iele .ne. 0) iflag_ele_tmp(iele) = -1
                end do
              end if
            end if
            if(iwidth .gt. 2) then
              if(iflag_ele(icel) .eq. 2) then
                ! forward
                call find_line_end_in_1ele(1, node%numnod, ele%numele, surf%numsurf,        &
                &      surf%nnod_4_surf, surf%isf_4_ele, surf%ie_surf, node%xx, icel, 0,    &
                &      v_start, x_start, isf_tgt, x_tgt, xi)
                !
                if(isf_tgt .ne. 0) then
                  isurf_end = abs(surf%isf_4_ele(icel,isf_tgt))
                  do i = 1, 2
                    iele = surf%iele_4_surf(isurf_end,i,1)
                    if(iele .ne. icel .and. iele .ne. 0) iflag_ele_tmp(iele) = 2
                  end do
                end if
              else if(iflag_ele(icel) .eq. -1) then
                ! backward
                call find_line_end_in_1ele(-1, node%numnod, ele%numele, surf%numsurf,        &
                &      surf%nnod_4_surf, surf%isf_4_ele, surf%ie_surf, node%xx, icel, 0,    &
                &      v_start, x_start, isf_tgt, x_tgt, xi)
                !
                if(isf_tgt .ne. 0) then
                  isurf_end = abs(surf%isf_4_ele(icel,isf_tgt))
                  do i = 1, 2
                    iele = surf%iele_4_surf(isurf_end,i,1)
                    if(iele .ne. icel .and. iele .ne. 0) iflag_ele_tmp(iele) = -1
                  end do
                end if
              end if
            end if
          end if
        end do
        iflag_ele(1:ele%numele) = iflag_ele_tmp(1:ele%numele)
      end do
!
      icou = 0
      item_tmp_e = 0
      do icel= 1, ele%numele
        if ( iflag_ele(icel) .ne. 0) then
          icou = icou + 1
          item_tmp_e(icou) = icel
        end if
      end do
      nele_subdomain = icou

deallocate(iflag_ele_tmp)

end subroutine selective_extended_overlap
!
!   --------------------------------------------------------------------
!
      end module increase_overlap
