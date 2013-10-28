!>@file   distribute_itp_tbl_4_para.f90
!!@brief  module distribute_itp_tbl_4_para
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Sep., 2013
!
!>@brief  Distribution interpolation table for parallel domains
!!
!!@verbatim
!!      subroutine const_parallel_itp_tbl(single_tbl, nprocs_table,     &
!!     &          para_tbl)
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
      use m_work_ditribute_itp
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
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
      subroutine const_parallel_itp_tbl(single_tbl, nprocs_table,       &
     &          para_tbl)
!
      type(interpolate_table), intent(in) :: single_tbl
!
      integer(kind = kint), intent(in) :: nprocs_table
      type(interpolate_table), intent(inout) :: para_tbl(nprocs_table)
!
      integer(kind = kint) :: ip, jp
!
!
      call count_itp_domain_4_para(nprocs_table, para_tbl)
!
      do ip = 1, nprocs_itp_dest
        call alloc_type_itp_num_dest(para_tbl(ip)%tbl_dest)
        call set_itp_dest_domain_4_para(ip, para_tbl(ip)%tbl_dest)
      end do
      do jp = 1, nprocs_itp_org
        call alloc_type_itp_num_org(para_tbl(jp)%tbl_org)
        call set_itp_org_domain_4_para(jp, para_tbl(jp)%tbl_org)
      end do
!
      do ip = 1, nprocs_itp_dest
        call count_itp_dest_tbl_4_para(ip, para_tbl(ip)%tbl_dest)
      end do
      do jp = 1, nprocs_itp_org
        call count_itp_org_tbl_4_para(jp, para_tbl(jp)%tbl_org)
      end do
!
      do ip = 1, nprocs_itp_dest
        call alloc_type_itp_table_dest(para_tbl(ip)%tbl_dest)
        call set_itp_dest_tbl_4_para(ip, para_tbl(ip)%tbl_dest)
      end do
      do jp = 1, nprocs_itp_org
        call alloc_type_itp_table_org(para_tbl(jp)%tbl_org)
        call set_itp_org_tbl_4_para(jp, single_tbl%tbl_org,             &
     &      para_tbl(jp)%tbl_org)
      end do
!
      do ip = nprocs_itp_dest+1, nprocs_table
        call alloc_type_zero_itp_tbl_dest(para_tbl(ip)%tbl_dest)
      end do
      do jp = nprocs_itp_org+1, nprocs_table
        call alloc_type_zero_itp_tbl_org(para_tbl(jp)%tbl_org)
      end do
!
      end subroutine const_parallel_itp_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_itp_domain_4_para(nprocs_table, para_tbl)
!
      integer(kind = kint), intent(in) :: nprocs_table
      type(interpolate_table), intent(inout) :: para_tbl(nprocs_table)
!
      integer(kind = kint) :: ip, jp, jp0, k, num
!
!
      para_tbl(1:nprocs_table)%tbl_dest%num_org_domain = 0
      para_tbl(1:nprocs_table)%tbl_org%num_dest_domain = 0
      do ip = 1, nprocs_itp_dest
        do jp0 = 1, nprocs_itp_org
          jp = mod((ip+jp0-1),nprocs_itp_org) + 1
          k = 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
          num = ntable_para(k+1) + ntable_para(k+2)                     &
     &         + ntable_para(k+3) + ntable_para(k+4)
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
      subroutine set_itp_dest_domain_4_para(ip, para_tbl_dest)
!
      integer(kind = kint), intent(in) :: ip
      type(interpolate_table_dest), intent(inout) :: para_tbl_dest
!
      integer(kind = kint) :: jp0, jp, k, ic, num
!
!
      ic = 0
      para_tbl_dest%iflag_self_itp_recv = 0
      do jp0 = 1, nprocs_itp_org
        jp = mod((ip+jp0-1),nprocs_itp_org) + 1
        k = 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
        num = ntable_para(k+1) + ntable_para(k+2)                     &
     &         + ntable_para(k+3) + ntable_para(k+4)
        if(num .gt. 0) then
          ic = ic + 1
          para_tbl_dest%id_org_domain(ic) = jp - 1
          if(ip .eq. jp)  para_tbl_dest%iflag_self_itp_recv = 1
        end if
      end do
!
      end subroutine set_itp_dest_domain_4_para
!
!-----------------------------------------------------------------------
!
      subroutine set_itp_org_domain_4_para(jp, para_tbl_org)
!
      integer(kind = kint), intent(in) :: jp
      type(interpolate_table_org), intent(inout) :: para_tbl_org
!
      integer(kind = kint) :: ip0, ip, k, ic, num
!
!
      ic = 0
      para_tbl_org%iflag_self_itp_send = 0
      do ip0 = 1, nprocs_itp_dest
        ip = mod(ip0+jp-1,nprocs_itp_dest) + 1
        k = 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
        num = ntable_para(k+1) + ntable_para(k+2)                       &
     &         + ntable_para(k+3) + ntable_para(k+4)
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
      subroutine count_itp_dest_tbl_4_para(ip, para_tbl_dest)
!
      integer(kind = kint), intent(in) :: ip
      type(interpolate_table_dest), intent(inout) :: para_tbl_dest
!
      integer(kind = kint) :: jp0, jp, i, j4, k4
!
!
      para_tbl_dest%istack_nod_tbl_dest(0) =       0
      para_tbl_dest%istack_nod_tbl_wtype_dest(0) = 0
      do jp0 = 1, para_tbl_dest%num_org_domain
        jp = para_tbl_dest%id_org_domain(jp0) + 1
        do i = 1, 4
          j4 = i + 4 * (jp0-1)
          k4 = i + 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
          para_tbl_dest%istack_nod_tbl_wtype_dest(j4)                   &
     &     = para_tbl_dest%istack_nod_tbl_wtype_dest(j4-1)              &
     &      + ntable_para(k4)
        end do
        para_tbl_dest%istack_nod_tbl_dest(jp0)                          &
     &     = para_tbl_dest%istack_nod_tbl_wtype_dest(4*jp0)
      end do
      jp0 = para_tbl_dest%num_org_domain
      para_tbl_dest%ntot_table_dest                                     &
     &     = para_tbl_dest%istack_nod_tbl_dest(jp0)
!      write(*,*) 'para_tbl_dest%ntot_table_dest',                      &
!     &            ip, para_tbl_dest%ntot_table_dest
!
      end subroutine count_itp_dest_tbl_4_para
!
!-----------------------------------------------------------------------
!
      subroutine count_itp_org_tbl_4_para(jp, para_tbl_org)
!
      integer(kind = kint), intent(in) :: jp
      type(interpolate_table_org), intent(inout) :: para_tbl_org
!
      integer(kind = kint) :: ip0, ip, i, i4, k4
!
!
      para_tbl_org%istack_nod_tbl_org(0) =       0
      para_tbl_org%istack_nod_tbl_wtype_org(0) = 0
      do ip0 = 1, para_tbl_org%num_dest_domain
        ip = para_tbl_org%id_dest_domain(ip0) + 1
        do i = 1, 4
          i4 = i + 4 * (ip0-1)
          k4 = i + 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
          para_tbl_org%istack_nod_tbl_wtype_org(i4)                     &
     &     = para_tbl_org%istack_nod_tbl_wtype_org(i4-1)                &
     &      + ntable_para(k4)
        end do
        para_tbl_org%istack_nod_tbl_org(ip0)                            &
     &     = para_tbl_org%istack_nod_tbl_wtype_org(4*ip0)
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
      subroutine set_itp_dest_tbl_4_para(ip, para_tbl_dest)
!
      integer(kind = kint), intent(in) :: ip
      type(interpolate_table_dest), intent(inout) :: para_tbl_dest
!
      integer(kind = kint) :: jp0, jp, i, j4, k, ist, ied, ic, inum
      integer(kind = kint) :: inod_gl
!
!
      do jp0 = 1, para_tbl_dest%num_org_domain
        jp = para_tbl_dest%id_org_domain(jp0) + 1
        do i = 1, 4
          j4 = i + 4*(jp0-1)
          k =  i + 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
          ist = istack_para(k-1) + 1
          ied = istack_para(k)
          ic =  para_tbl_dest%istack_nod_tbl_wtype_dest(j4-1)
          do inum = ist, ied
            ic = ic + 1
            inod_gl = itable_para_order(inum)
            para_tbl_dest%inod_dest_4_dest(ic)                          &
     &                = inod_lc_dest(inod_gl,1)
          end do
        end do
      end do
!
      do ic = 1, para_tbl_dest%ntot_table_dest
        if(para_tbl_dest%inod_dest_4_dest(ic) .eq. 0) then
          write(*,*) 'targe node ', ic, ' is missing on domain ', ip
        end if
      end do
!
      end subroutine set_itp_dest_tbl_4_para
!
!-----------------------------------------------------------------------
!
      subroutine set_itp_org_tbl_4_para(jp, sgl_tbl_org, para_tbl_org)
!
      integer(kind = kint), intent(in) :: jp
      type(interpolate_table_org), intent(in) ::    sgl_tbl_org
      type(interpolate_table_org), intent(inout) :: para_tbl_org
!
      integer(kind = kint) :: ip0, ip, i, i4, k, ist, ied, ic, inum
      integer(kind = kint) :: inod_gl, iele_gl, irev_gl
!
!
      do ip0 = 1, para_tbl_org%num_dest_domain
        ip = para_tbl_org%id_dest_domain(ip0) + 1
        do i = 1, 4
          i4 = i + 4 * (ip0-1)
          k =  i + 4 * (jp-1) + 4*nprocs_itp_org * (ip-1)
          ist = istack_para(k-1) + 1
          ied = istack_para(k)
          ic =  para_tbl_org%istack_nod_tbl_wtype_org(i4-1)
          do inum = ist, ied
            ic = ic + 1
            inod_gl = itable_para_order(inum)
            irev_gl = irev_tbl_org(inod_gl)
            iele_gl = sgl_tbl_org%iele_org_4_org(irev_gl)
!
            para_tbl_org%inod_itp_send(ic) =      ic
            para_tbl_org%inod_gl_dest_4_org(ic) = inod_gl
            para_tbl_org%iele_org_4_org(ic) = iele_lc_org(iele_gl,1)
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
