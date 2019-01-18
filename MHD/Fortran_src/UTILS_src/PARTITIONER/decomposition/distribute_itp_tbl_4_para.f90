!>@file   distribute_itp_tbl_4_para.f90
!!@brief  module distribute_itp_tbl_4_para
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Sep., 2013
!
!>@brief  Distribution interpolation table for parallel domains
!!
!!@verbatim
!!      subroutine const_parallel_itp_tbl(single_tbl, wk_dist_itp,      &
!!     &          nprocs_table, para_tbl)
!!        type(interpolate_table), intent(in) :: single_tbl
!!        type(work_ditribute_itp), intent(in) :: wk_dist_itp
!!        type(interpolate_table), intent(inout)                        &
!!                                :: para_tbl(nprocs_table)
!!@endverbatim
!!
!!@n @param  single_tbl    interpolation table for single domain
!!@n @param  nprocs_table  number of subdomains 
!!                         for parallel interpolation
!!@n @param  para_tbl      interpolation table for ditributed domains
!
      module distribute_itp_tbl_4_para
!
      use m_precision
!
      use t_work_ditribute_itp
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
!> Structure of interpolation table for target grid
      type itp_stack_dest_wk_para
!>   end address to receive interpolated data including interpolate type
        integer(kind = kint), allocatable :: istack_tbl_wt_dest(:)
      end type itp_stack_dest_wk_para
!
      private :: alloc_itp_stack_dest_wk_para
      private ::  dealloc_itp_stack_dest_wk_para
      private :: count_itp_domain_4_para
      private :: set_itp_dest_domain_4_para, set_itp_org_domain_4_para
      private :: count_itp_dest_tbl_4_para,  count_itp_org_tbl_4_para
      private :: set_itp_dest_tbl_4_para, set_itp_org_tbl_4_para
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_itp_stack_dest_wk_para(num_org_domain, dest_wk)
!
      integer(kind = kint), intent(in) :: num_org_domain
      type(itp_stack_dest_wk_para), intent(inout) :: dest_wk
!
!
      allocate( dest_wk%istack_tbl_wt_dest(0:4*num_org_domain) )
      dest_wk%istack_tbl_wt_dest = 0
!
      end subroutine alloc_itp_stack_dest_wk_para
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_stack_dest_wk_para(dest_wk)
!
      type(itp_stack_dest_wk_para), intent(inout) :: dest_wk
!
!
      deallocate( dest_wk%istack_tbl_wt_dest )
!
      end subroutine dealloc_itp_stack_dest_wk_para
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_parallel_itp_tbl(single_tbl, wk_dist_itp,        &
     &          nprocs_table, para_tbl)
!
      use m_machine_parameter
      use m_work_const_itp_table
      use ordering_itp_org_tbl
!
      type(interpolate_table), intent(in) :: single_tbl
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
!
      integer(kind = kint), intent(in) :: nprocs_table
      type(interpolate_table), intent(inout)                            &
                              :: para_tbl(nprocs_table)
!
      type(itp_stack_dest_wk_para), allocatable :: dest_wk(:)
!
      integer(kind = kint) :: ip, jp
!
!
      call count_itp_domain_4_para(nprocs_table, wk_dist_itp, para_tbl)
!
      do ip = 1, wk_dist_itp%nprocs_itp_dest
        call alloc_itp_num_dest(para_tbl(ip)%tbl_dest)
        call set_itp_dest_domain_4_para                                 &
     &     (ip, wk_dist_itp, para_tbl(ip)%tbl_dest)
      end do
      do jp = 1, wk_dist_itp%nprocs_itp_org
        call alloc_itp_num_org(np_smp, para_tbl(jp)%tbl_org)
        call set_itp_org_domain_4_para                                  &
     &     (jp, wk_dist_itp, para_tbl(jp)%tbl_org)
      end do
!
!
      allocate( dest_wk(wk_dist_itp%nprocs_itp_dest) )
      do ip = 1, wk_dist_itp%nprocs_itp_dest
        call alloc_itp_stack_dest_wk_para                               &
     &     (para_tbl(ip)%tbl_dest%num_org_domain, dest_wk(ip))
        call count_itp_dest_tbl_4_para(ip, wk_dist_itp, dest_wk(ip),    &
            para_tbl(ip)%tbl_dest)
      end do
      do jp = 1, wk_dist_itp%nprocs_itp_org
        call count_itp_org_tbl_4_para                                   &
     &     (jp, wk_dist_itp, para_tbl(jp)%tbl_org)
      end do
!
      do ip = 1, wk_dist_itp%nprocs_itp_dest
        call alloc_itp_table_dest(para_tbl(ip)%tbl_dest)
        call set_itp_dest_tbl_4_para(ip, dest_wk(ip), wk_dist_itp,      &
     &      para_tbl(ip)%tbl_dest)
!
        call dealloc_itp_stack_dest_wk_para(dest_wk(ip))
      end do
      deallocate(dest_wk)
!
!
      do jp = 1, wk_dist_itp%nprocs_itp_org
        call allocate_istack_org_ptype                                  &
     &     (para_tbl((jp))%tbl_org%num_dest_domain)
        call alloc_itp_table_org(para_tbl(jp)%tbl_org)
        call set_itp_org_tbl_4_para(jp, wk_dist_itp,                    &
     &      single_tbl%tbl_org, para_tbl(jp)%tbl_org)
        call ordering_itp_orgin_tbl_t(para_tbl(jp)%tbl_org)
        call deallocate_istack_org_ptype
      end do
!
      do ip = wk_dist_itp%nprocs_itp_dest+1, nprocs_table
        call alloc_type_zero_itp_tbl_dest(para_tbl(ip)%tbl_dest)
      end do
      do jp = wk_dist_itp%nprocs_itp_org+1, nprocs_table
        call alloc_zero_itp_tbl_org(np_smp, para_tbl(jp)%tbl_org)
      end do
!
      end subroutine const_parallel_itp_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_itp_domain_4_para                                &
     &         (nprocs_table, wk_dist_itp, para_tbl)
!
      integer(kind = kint), intent(in) :: nprocs_table
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table), intent(inout) :: para_tbl(nprocs_table)
!
      integer(kind = kint) :: ip, jp, jp0, k, num
!
!
      para_tbl(1:nprocs_table)%tbl_dest%num_org_domain = 0
      para_tbl(1:nprocs_table)%tbl_org%num_dest_domain = 0
      do ip = 1, wk_dist_itp%nprocs_itp_dest
        do jp0 = 1, wk_dist_itp%nprocs_itp_org
          jp = mod((ip+jp0-1),wk_dist_itp%nprocs_itp_org) + 1
          k = 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          num =  wk_dist_itp%ntable_para(k+1)                           &
     &         + wk_dist_itp%ntable_para(k+2)                           &
     &         + wk_dist_itp%ntable_para(k+3)                           &
     &         + wk_dist_itp%ntable_para(k+4)
          if(num .gt. 0) then
            para_tbl(ip)%tbl_dest%num_org_domain                        &
     &           =  para_tbl(ip)%tbl_dest%num_org_domain + 1
            para_tbl(jp)%tbl_org%num_dest_domain                        &
     &           =  para_tbl(jp)%tbl_org%num_dest_domain + 1
          end if
        end do
      end do
!
      end subroutine count_itp_domain_4_para
!
!-----------------------------------------------------------------------
!
      subroutine set_itp_dest_domain_4_para                             &
     &         (ip, wk_dist_itp, itp_dst_para)
!
      integer(kind = kint), intent(in) :: ip
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table_dest), intent(inout) :: itp_dst_para
!
      integer(kind = kint) :: jp0, jp, k, ic, num
!
!
      ic = 0
      itp_dst_para%iflag_self_itp_recv = 0
      do jp0 = 1, wk_dist_itp%nprocs_itp_org
        jp = mod((ip+jp0-1),wk_dist_itp%nprocs_itp_org) + 1
        k = 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
        num =  wk_dist_itp%ntable_para(k+1)                             &
     &       + wk_dist_itp%ntable_para(k+2)                             &
     &       + wk_dist_itp%ntable_para(k+3)                             &
     &       + wk_dist_itp%ntable_para(k+4)
        if(num .gt. 0) then
          ic = ic + 1
          itp_dst_para%id_org_domain(ic) = jp - 1
          if(ip .eq. jp)  itp_dst_para%iflag_self_itp_recv = 1
        end if
      end do
!
      end subroutine set_itp_dest_domain_4_para
!
!-----------------------------------------------------------------------
!
      subroutine set_itp_org_domain_4_para                              &
     &         (jp, wk_dist_itp, para_tbl_org)
!
      integer(kind = kint), intent(in) :: jp
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table_org), intent(inout) :: para_tbl_org
!
      integer(kind = kint) :: ip0, ip, k, ic, num
!
!
      ic = 0
      para_tbl_org%iflag_self_itp_send = 0
      do ip0 = 1, wk_dist_itp%nprocs_itp_dest
        ip = mod(ip0+jp-1,wk_dist_itp%nprocs_itp_dest) + 1
        k = 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
        num =  wk_dist_itp%ntable_para(k+1)                             &
     &       + wk_dist_itp%ntable_para(k+2)                             &
     &       + wk_dist_itp%ntable_para(k+3)                             &
     &       + wk_dist_itp%ntable_para(k+4)
        if(num .gt. 0) then
          ic = ic + 1
          para_tbl_org%id_dest_domain(ic) = ip - 1
          if(ip .eq. jp) para_tbl_org%iflag_self_itp_send = 1
        end if
      end do
!
      end subroutine set_itp_org_domain_4_para
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_itp_dest_tbl_4_para                              &
     &         (ip, wk_dist_itp, dest_wk, itp_dst_para)
!
      integer(kind = kint), intent(in) :: ip
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table_dest), intent(inout) :: itp_dst_para
      type(itp_stack_dest_wk_para), intent(inout) :: dest_wk
!
      integer(kind = kint) :: jp0, jp, i, j4, k4
!
!
      dest_wk%istack_tbl_wt_dest(0) = 0
      do jp0 = 1, itp_dst_para%num_org_domain
        jp = itp_dst_para%id_org_domain(jp0) + 1
        do i = 1, 4
          j4 = i + 4 * (jp0-1)
          k4 = i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          dest_wk%istack_tbl_wt_dest(j4)                                &
     &     = dest_wk%istack_tbl_wt_dest(j4-1)                           &
     &      + wk_dist_itp%ntable_para(k4)
        end do
      end do
!
      itp_dst_para%istack_nod_tbl_dest(0) = 0
      do jp0 = 1, itp_dst_para%num_org_domain
        itp_dst_para%istack_nod_tbl_dest(jp0)                           &
     &     = dest_wk%istack_tbl_wt_dest(4*jp0)
      end do
      jp0 = itp_dst_para%num_org_domain
      itp_dst_para%ntot_table_dest                                      &
     &    = itp_dst_para%istack_nod_tbl_dest(jp0)
!
      end subroutine count_itp_dest_tbl_4_para
!
!-----------------------------------------------------------------------
!
      subroutine count_itp_org_tbl_4_para                               &
     &         (jp, wk_dist_itp, para_tbl_org)
!
      integer(kind = kint), intent(in) :: jp
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table_org), intent(inout) :: para_tbl_org
!
      integer(kind = kint) :: ip0, ip, kst
!
!
      para_tbl_org%istack_nod_tbl_org(0) =       0
      do ip0 = 1, para_tbl_org%num_dest_domain
        ip = para_tbl_org%id_dest_domain(ip0) + 1
        kst = 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
!
        para_tbl_org%istack_nod_tbl_org(ip0)                            &
     &     = para_tbl_org%istack_nod_tbl_org(ip0-1)                     &
     &      + wk_dist_itp%ntable_para(kst+1)                            &
     &      + wk_dist_itp%ntable_para(kst+2)                            &
     &      + wk_dist_itp%ntable_para(kst+3)                            &
     &      + wk_dist_itp%ntable_para(kst+4)
      end do
      ip0 = para_tbl_org%num_dest_domain
      para_tbl_org%ntot_table_org                                       &
     &     = para_tbl_org%istack_nod_tbl_org(ip0)
!      write(*,*) 'para_tbl_org%ntot_table_org',                        &
!     &           jp, para_tbl_org%ntot_table_org
!
      end subroutine count_itp_org_tbl_4_para
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_itp_dest_tbl_4_para                                &
     &         (ip, dest_wk, wk_dist_itp, itp_dst_para)
!
      integer(kind = kint), intent(in) :: ip
      type(itp_stack_dest_wk_para), intent(in) :: dest_wk
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table_dest), intent(inout) :: itp_dst_para
!
      integer(kind = kint) :: jp0, jp, i, j4, k, ist, ied, ic, inum
      integer(kind = kint) :: inod_gl
!
!
      do jp0 = 1, itp_dst_para%num_org_domain
        jp = itp_dst_para%id_org_domain(jp0) + 1
        do i = 1, 4
          j4 = i + 4*(jp0-1)
          k =  i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          ist = wk_dist_itp%istack_para(k-1) + 1
          ied = wk_dist_itp%istack_para(k)
          ic =  dest_wk%istack_tbl_wt_dest(j4-1)
          do inum = ist, ied
            ic = ic + 1
            inod_gl = wk_dist_itp%itable_para_order(inum)
            itp_dst_para%inod_dest_4_dest(ic)                           &
     &                = wk_dist_itp%inod_lc_dest(inod_gl,1)
          end do
        end do
      end do
!
      do ic = 1, itp_dst_para%ntot_table_dest
        if(itp_dst_para%inod_dest_4_dest(ic) .eq. 0) then
          write(*,*) 'targe node ', ic, ' is missing on domain ', ip
        end if
      end do
!
      end subroutine set_itp_dest_tbl_4_para
!
!-----------------------------------------------------------------------
!
      subroutine set_itp_org_tbl_4_para                                 &
     &         (jp, wk_dist_itp, sgl_tbl_org, para_tbl_org)
!
      use m_work_const_itp_table
!
      integer(kind = kint), intent(in) :: jp
      type(work_ditribute_itp), intent(in) :: wk_dist_itp
      type(interpolate_table_org), intent(in) ::    sgl_tbl_org
      type(interpolate_table_org), intent(inout) :: para_tbl_org
!
      integer(kind = kint) :: ip0, ip, i, i4, k, ist, ied, ic, inum, k4
      integer(kind = kint) :: inod_gl, iele_gl, irev_gl
!
!
      istack_org_para_type(0) = 0
      do ip0 = 1, para_tbl_org%num_dest_domain
        ip = para_tbl_org%id_dest_domain(ip0) + 1
        do i = 1, 4
          i4 = i + 4 * (ip0-1)
          k4 = i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          istack_org_para_type(i4) = istack_org_para_type(i4-1)         &
     &                              + wk_dist_itp%ntable_para(k4)
        end do
      end do
!
      do ip0 = 1, para_tbl_org%num_dest_domain
        ip = para_tbl_org%id_dest_domain(ip0) + 1
        do i = 1, 4
          i4 = i + 4 * (ip0-1)
          k =  i + 4 * (jp-1) + 4*wk_dist_itp%nprocs_itp_org * (ip-1)
          ist = wk_dist_itp%istack_para(k-1) + 1
          ied = wk_dist_itp%istack_para(k)
          ic =  istack_org_para_type(i4-1)
          do inum = ist, ied
            ic = ic + 1
            inod_gl = wk_dist_itp%itable_para_order(inum)
            irev_gl = wk_dist_itp%irev_tbl_org(inod_gl)
            iele_gl = sgl_tbl_org%iele_org_4_org(irev_gl)
!
            para_tbl_org%inod_itp_send(ic) =      ic
            para_tbl_org%inod_gl_dest_4_org(ic) = inod_gl
            para_tbl_org%iele_org_4_org(ic)                             &
     &               = wk_dist_itp%iele_lc_org(iele_gl,1)
            para_tbl_org%itype_inter_org(ic)                            &
     &               = sgl_tbl_org%itype_inter_org(irev_gl)
!
            para_tbl_org%coef_inter_org(ic,1)                           &
     &               = sgl_tbl_org%coef_inter_org(irev_gl,1)
            para_tbl_org%coef_inter_org(ic,2)                           &
     &               = sgl_tbl_org%coef_inter_org(irev_gl,2)
            para_tbl_org%coef_inter_org(ic,3)                           &
     &               = sgl_tbl_org%coef_inter_org(irev_gl,3)
          end do
        end do
      end do
!
      do ic = 1, para_tbl_org%ntot_table_org
        if(para_tbl_org%iele_org_4_org(ic) .eq. 0) then
          write(*,*) 'original element ', ic,                           &
     &              ' is missing on domain ', jp
        end if
      end do
!
      end subroutine set_itp_org_tbl_4_para
!
!-----------------------------------------------------------------------
!
      end module distribute_itp_tbl_4_para
