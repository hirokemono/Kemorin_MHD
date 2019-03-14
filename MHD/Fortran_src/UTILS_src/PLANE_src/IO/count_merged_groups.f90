!
!      module count_merged_groups
!
!      subroutine count_num_merged_grp(num_pe, sub, istack_grp,         &
!     &          ntot_grp)
!        integer, intent(in) :: num_pe
!        integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
!        type(group_data), intent(in) :: sub(num_pe)
!        integer (kind = kint), intent(inout) :: ntot_grp
!      subroutine count_num_merged_sf_grp(num_pe, sub_sf, istack_grp,   &
!     &          ntot_grp)
!        integer, intent(in) :: num_pe
!        integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
!        type(surface_group_data), intent(in) :: sub_sf(num_pe)
!        integer (kind = kint), intent(inout) :: ntot_grp
!
!      subroutine set_merged_grp_name(num_pe, sub, istack_grp,          &
!     &          merged_gp)
!        integer, intent(in) :: num_pe
!        integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
!        type(group_data), intent(in) :: sub(num_pe)
!        type(group_data), intent(inout) :: merged_gp
!      subroutine set_merged_grp_sf_name(num_pe, sub_sf, istack_grp,    &
!     &          merged_gp)
!        integer, intent(in) :: num_pe
!        integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
!        type(surface_group_data), intent(in) :: sub_sf(num_pe)
!        type(surface_group_data), intent(inout) :: merged_gp
!
!      Written by H. Matsui on july, 2005
!
      module count_merged_groups
!
      use m_precision
!
      use m_constants
!
      implicit none
!
      integer (kind = kint), allocatable, private :: iflag_mk(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_num_merged_grp(num_pe, sub, istack_grp,          &
     &          ntot_grp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
      type(group_data), intent(in) :: sub(num_pe)
!
      integer (kind = kint), intent(inout) :: ntot_grp
!
      integer (kind = kint) :: ip, igrp, igrp_st
      integer (kind = kint) :: jp, jgrp, jgrp_st
!
!
      allocate( iflag_mk(istack_grp(num_pe)) )
      iflag_mk = 0
!
      ntot_grp = 0
      do ip = 1, num_pe
        igrp_st = istack_grp(ip-1)
        do igrp = 1, sub(ip)%num_grp
          if(iflag_mk(igrp+igrp_st) .eq. 0) then
            ntot_grp = ntot_grp + 1
!
            do jp = ip+1, num_pe
              jgrp_st = istack_grp(jp-1)
              do jgrp = 1, sub(jp)%num_grp
                if(sub(jp)%grp_name(jgrp)                               &
     &              .eq. sub(ip)%grp_name(igrp)) then
                  iflag_mk(jgrp+jgrp_st) = ntot_grp
                end if
              end do
            end do
!
          end if
        end do
      end do
      deallocate( iflag_mk )
!
      end subroutine count_num_merged_grp
!
!------------------------------------------------------------------
!
      subroutine count_num_merged_sf_grp(num_pe, sub_sf, istack_grp,    &
     &          ntot_grp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
      type(surface_group_data), intent(in) :: sub_sf(num_pe)
!
      integer (kind = kint), intent(inout) :: ntot_grp
!
      integer (kind = kint) :: ip, igrp, igrp_st
      integer (kind = kint) :: jp, jgrp, jgrp_st
!
!
      allocate( iflag_mk(istack_grp(num_pe)) )
      iflag_mk = 0
!
      ntot_grp = 0
      do ip = 1, num_pe
        igrp_st = istack_grp(ip-1)
        do igrp = 1, sub_sf(ip)%num_grp
          if(iflag_mk(igrp+igrp_st) .eq. 0) then
            ntot_grp = ntot_grp + 1
!
            do jp = ip+1, num_pe
              jgrp_st = istack_grp(jp-1)
              do jgrp = 1, sub_sf(jp)%num_grp
                if(sub_sf(jp)%grp_name(jgrp)                            &
     &              .eq. sub_sf(ip)%grp_name(igrp)) then
                  iflag_mk(jgrp+jgrp_st) = ntot_grp
                end if
              end do
            end do
!
          end if
        end do
      end do
      deallocate( iflag_mk )
!
      end subroutine count_num_merged_sf_grp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_merged_grp_name(num_pe, sub, istack_grp,           &
     &          merged_gp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
      type(group_data), intent(in) :: sub(num_pe)
!
      type(group_data), intent(inout) :: merged_gp
!
      integer (kind = kint) :: ip, igrp, igrp_st
      integer (kind = kint) :: jp, jgrp, jgrp_st, icou
!
!
      allocate( iflag_mk(istack_grp(num_pe)) )
      iflag_mk = 0
!
      icou = 0
      do ip = 1, num_pe
        igrp_st = istack_grp(ip-1)
        do igrp = 1, sub(ip)%num_grp
          if(iflag_mk(igrp+igrp_st) .eq. 0) then
            icou = icou + 1
            merged_gp%grp_name(icou) = sub(ip)%grp_name(igrp)
            if(icou .eq. merged_gp%num_grp) then
              deallocate( iflag_mk )
              return
            end if
!
            do jp = ip+1, num_pe
              jgrp_st = istack_grp(jp-1)
              do jgrp = 1, sub(jp)%num_grp
                if(sub(jp)%grp_name(jgrp)                               &
     &              .eq. sub(ip)%grp_name(igrp)) then
                  iflag_mk(jgrp+jgrp_st) = icou
                end if
              end do
            end do
!
          end if
        end do
      end do
      deallocate( iflag_mk )
!
      end subroutine set_merged_grp_name
!
!------------------------------------------------------------------
!
      subroutine set_merged_grp_sf_name(num_pe, sub_sf, istack_grp,     &
     &          merged_gp)
!
      use t_group_data
!
      integer, intent(in) :: num_pe
      integer (kind = kint), intent(in) :: istack_grp(0:num_pe)
      type(surface_group_data), intent(in) :: sub_sf(num_pe)
!
      type(surface_group_data), intent(inout) :: merged_gp
!
      integer (kind = kint) :: ip, igrp, igrp_st
      integer (kind = kint) :: jp, jgrp, jgrp_st, icou
!
!
      allocate( iflag_mk(istack_grp(num_pe)) )
      iflag_mk = 0
!
      icou = 0
      do ip = 1, num_pe
        igrp_st = istack_grp(ip-1)
        do igrp = 1, sub_sf(ip)%num_grp
          if(iflag_mk(igrp+igrp_st) .eq. 0) then
            icou = icou + 1
            merged_gp%grp_name(icou) = sub_sf(ip)%grp_name(igrp)
            if(icou .eq. merged_gp%num_grp) then
              deallocate( iflag_mk )
              return
            end if
!
            do jp = ip+1, num_pe
              jgrp_st = istack_grp(jp-1)
              do jgrp = 1, sub_sf(jp)%num_grp
                if(sub_sf(jp)%grp_name(jgrp)                            &
     &              .eq. sub_sf(ip)%grp_name(igrp)) then
                  iflag_mk(jgrp+jgrp_st) = icou
                end if
              end do
            end do
!
          end if
        end do
      end do
      deallocate( iflag_mk )
!
      end subroutine set_merged_grp_sf_name
!
!------------------------------------------------------------------
!
      end module count_merged_groups
