!set_overlap_groups.f90
!      module set_overlap_groups
!
!      Written by H. Matsui on july, 2005
!
!      subroutine count_group_w_overlap(sub_grp, mgd_grp)
!      subroutine set_group_w_overlap(sub_grp, mgd_grp)
!      subroutine count_surf_group_w_overlap(sub_grp, mgd_grp)
!      subroutine set_surf_group_w_overlap(sub_grp, mgd_grp)
!
      module set_overlap_groups
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_group_w_overlap(num_pe, sub_grp, mgd_grp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      type(group_data), intent(in) :: sub_grp(num_pe)
      type(group_data), intent(inout) :: mgd_grp
!
      integer (kind = kint) :: i, j, ip, icou
!
!
      mgd_grp%istack_grp(0) = 0
      do j = 1, mgd_grp%num_grp
!
        icou = mgd_grp%istack_grp(j-1)
        do ip = 1, num_pe
          do i = 1, sub_grp(ip)%num_grp
            if ( sub_grp(ip)%grp_name(i) .eq. mgd_grp%grp_name(j) )  then
              icou = icou + sub_grp(ip)%istack_grp(i  )                 &
     &                    - sub_grp(ip)%istack_grp(i-1)
              exit
            end if
          end do
        end do
        mgd_grp%istack_grp(j) = icou
!
      end do
      mgd_grp%num_item = mgd_grp%istack_grp(mgd_grp%num_grp)
!
      end subroutine count_group_w_overlap
!
!-----------------------------------------------------------------------
!
      subroutine set_group_w_overlap(num_pe, sub_grp, mgd_grp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      type(group_data), intent(in) :: sub_grp(num_pe)
      type(group_data), intent(inout) :: mgd_grp
!
      integer (kind = kint) :: i, j, ip, k, icou, kst, ked
!
!
      do j = 1, mgd_grp%num_grp
!
        icou = mgd_grp%istack_grp(j-1)
        do ip = 1, num_pe
          do i = 1, sub_grp(ip)%num_grp
            if ( sub_grp(ip)%grp_name(i) .eq. mgd_grp%grp_name(j) )  then
              kst = sub_grp(ip)%istack_grp(i-1)+1
              ked = sub_grp(ip)%istack_grp(i  )
              do k = kst, ked
                icou = icou + 1
                mgd_grp%item_grp(icou) = sub_grp(ip)%item_grp(k)
              end do
              exit
            end if
          end do
        end do
!
      end do
!
      end subroutine set_group_w_overlap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_surf_group_w_overlap(num_pe, sub_grp, mgd_grp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      type(surface_group_data), intent(in) :: sub_grp(num_pe)
      type(surface_group_data), intent(inout) :: mgd_grp
!
      integer (kind = kint) :: i, j, ip, icou
!
!
      mgd_grp%istack_grp(0) = 0
      do j = 1, mgd_grp%num_grp
!
        icou = mgd_grp%istack_grp(j-1)
        do ip = 1, num_pe
          do i = 1, sub_grp(ip)%num_grp
            if ( sub_grp(ip)%grp_name(i) .eq. mgd_grp%grp_name(j) )  then
              icou = icou + sub_grp(ip)%istack_grp(i  )                 &
     &                    - sub_grp(ip)%istack_grp(i-1)
              exit
            end if
          end do
        end do
        mgd_grp%istack_grp(j) = icou
!
      end do
      mgd_grp%num_item = mgd_grp%istack_grp(mgd_grp%num_grp)
!
      end subroutine count_surf_group_w_overlap
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_group_w_overlap(num_pe, sub_grp, mgd_grp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      type(surface_group_data), intent(in) :: sub_grp(num_pe)
      type(surface_group_data), intent(inout) :: mgd_grp
!
      integer (kind = kint) :: i, j, ip, k, icou, kst, ked
!
!
      do j = 1, mgd_grp%num_grp
!
        icou = mgd_grp%istack_grp(j-1)
        do ip = 1, num_pe
          do i = 1, sub_grp(ip)%num_grp
            if ( sub_grp(ip)%grp_name(i) .eq. mgd_grp%grp_name(j) )  then
              kst = sub_grp(ip)%istack_grp(i-1)+1
              ked = sub_grp(ip)%istack_grp(i  )
              do k = kst, ked
                icou = icou + 1
                mgd_grp%item_sf_grp(1,icou)                             &
     &                      =  sub_grp(ip)%item_sf_grp(1,k)
                mgd_grp%item_sf_grp(2,icou)                             &
     &                      =  sub_grp(ip)%item_sf_grp(2,k)
              end do
              exit
            end if
          end do
        end do
!
      end do
!
      end subroutine set_surf_group_w_overlap
!
!-----------------------------------------------------------------------
!
      end module set_overlap_groups
