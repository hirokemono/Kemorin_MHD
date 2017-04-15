!>@file   t_sph_mesh_1d_connect.F90
!!@brief  module t_sph_mesh_1d_connect
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief  One-dimmentional connectivity list for spherical shell
!!
!!@verbatim
!!      subroutine alloc_radius_1d_gl(nri_global, tbl)
!!      subroutine dealloc_radius_1d_gl(tbl)
!!
!!      subroutine alloc_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,&
!!     &          nidx_global_rtp, m_folding, tbl)
!!      subroutine alloc_iele_sph_mesh(tbl)
!!      subroutine dealloc_nnod_nele_sph_mesh(tbl)
!!
!!      subroutine check_iele_4_sph_connects(tbl)
!!@endverbatim
!
      module t_sph_mesh_1d_connect
!
      use m_precision
      use m_constants
!
      implicit none
!
      type comm_table_make_sph
        integer(kind = kint) :: ntot_domain
        integer(kind = kint) :: ndomain_fem(3)
        integer(kind = kint) :: nidx_global_fem(3)
        integer(kind = kint) :: nidx_local_fem(3)
!
!>        global radius data @f$ r(k) @f$
        real(kind = kreal), allocatable :: radius_1d_gl(:)
!
        integer(kind = kint), allocatable :: iflag_neib_r(:,:)
        integer(kind = kint), allocatable :: iflag_neib_t(:,:)
!
        integer(kind = kint), allocatable :: item_import_rtp(:)
        integer(kind = kint), allocatable :: item_export_rtp(:)
        integer(kind = kint), allocatable :: item_import_1d_rtp(:,:)
        integer(kind = kint), allocatable :: item_export_1d_rtp(:,:)
!
        integer(kind = kint), allocatable :: iflag_internal_r(:,:)
        integer(kind = kint), allocatable :: iflag_internal_t(:,:)
!
        integer(kind = kint), allocatable :: iflag_Spole_t(:)
        integer(kind = kint), allocatable :: iflag_Npole_t(:)
        integer(kind = kint), allocatable :: iflag_center_r(:)
        integer(kind = kint), allocatable :: iflag_center_t(:)
!
        integer(kind = kint), allocatable :: iflag_ele_r(:,:)
        integer(kind = kint), allocatable :: iflag_ele_t(:,:)
!
        integer(kind = kint), allocatable :: iflag_ele_Spole(:)
        integer(kind = kint), allocatable :: iflag_ele_Npole(:)
        integer(kind = kint), allocatable :: iflag_ele_center(:)
!
        integer(kind = kint), allocatable :: nnod_sph_r(:)
        integer(kind = kint), allocatable :: nnod_sph_t(:)
!
        integer(kind = kint), allocatable :: nele_sph_r(:)
        integer(kind = kint), allocatable :: nele_sph_t(:)
!
        integer(kind = kint) :: nmax_nod_sph_r
        integer(kind = kint) :: nmax_nod_sph_t
!  1D global node address
        integer(kind = kint), allocatable :: inod_sph_r(:,:)
        integer(kind = kint), allocatable :: inod_sph_t(:,:)
!  1D local node address at global node address
        integer(kind = kint), allocatable :: irev_sph_r(:,:)
        integer(kind = kint), allocatable :: irev_sph_t(:,:)
!
        integer(kind = kint) :: nnod_sph_ct
        integer(kind = kint), allocatable :: inod_sph_ct(:)
        integer(kind = kint), allocatable :: irev_sph_ct(:)
!
        integer(kind = kint) :: nmax_ele_sph_r
        integer(kind = kint) :: nmax_ele_sph_t
!
!  1D element address using local 1d node ID
        integer(kind = kint), allocatable :: ie_sph_r(:,:,:)
        integer(kind = kint), allocatable :: ie_sph_t(:,:,:)
        integer(kind = kint), allocatable :: ie_sph_p(:,:)
!
        integer(kind = kint) :: nele_around_pole
        integer(kind = kint), allocatable :: ie_Spole_t(:,:)
        integer(kind = kint), allocatable :: ie_Npole_t(:,:)
        integer(kind = kint), allocatable :: ie_center_r(:,:)
        integer(kind = kint), allocatable :: ie_center_t(:,:)
        integer(kind = kint) :: ie_center_Sp(2)
        integer(kind = kint) :: ie_center_Np(2)
      end type comm_table_make_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_radius_1d_gl(nri_global, tbl)
!
      integer(kind = kint), intent(in) :: nri_global
      type(comm_table_make_sph), intent(inout) :: tbl
!
!
      allocate(tbl%radius_1d_gl(nri_global))
      if(nri_global .gt. 0) tbl%radius_1d_gl = 0.0d0
!
      end subroutine alloc_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_radius_1d_gl(tbl)
!
      type(comm_table_make_sph), intent(inout) :: tbl
!
      deallocate(tbl%radius_1d_gl)
!
      end subroutine dealloc_radius_1d_gl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,     &
     &          nidx_global_rtp, m_folding, tbl)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint), intent(in) :: ndomain_rtp(3)
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
      integer(kind = kint), intent(in) :: m_folding
!
      type(comm_table_make_sph), intent(inout) :: tbl
!
      integer(kind = kint) :: np, num
!
!
      tbl%ntot_domain =      ndomain_sph
      tbl%ndomain_fem(1:3) = ndomain_rtp(1:3)
      tbl%nidx_global_fem(1:3) = nidx_global_rtp(1:3)
      tbl%nidx_global_fem(3) =  m_folding * tbl%nidx_global_fem(3)
!
      np =  tbl%ndomain_fem(1)
      num = tbl%nidx_global_fem(1)
      allocate( tbl%iflag_neib_r(np,np) )
      allocate( tbl%iflag_ele_r(num-1,np) )
      allocate( tbl%iflag_internal_r(num,np) )
      allocate( tbl%nnod_sph_r(np) )
      allocate( tbl%nele_sph_r(np) )
      tbl%iflag_neib_r =   0
      tbl%iflag_internal_r = 0
      tbl%iflag_ele_r = 0
      tbl%nnod_sph_r = 0
      tbl%nele_sph_r = 0
!
      allocate( tbl%iflag_center_r(np) )
      allocate( tbl%iflag_ele_center(np) )
      tbl%iflag_center_r =   0
      tbl%iflag_ele_center = 0
!
      np =  tbl%ndomain_fem(2)
      num = tbl%nidx_global_fem(2)
      allocate( tbl%iflag_neib_t(np,np) )
      allocate( tbl%iflag_internal_t(num,np) )
      allocate( tbl%iflag_ele_t(num-1,np) )
      allocate( tbl%nnod_sph_t(np) )
      allocate( tbl%nele_sph_t(np) )
      tbl%iflag_neib_t =   0
      tbl%iflag_internal_t = 0
      tbl%iflag_ele_t = 0
      tbl%nnod_sph_t = 0
      tbl%nele_sph_t = 0
!
      allocate( tbl%iflag_Spole_t(np) )
      allocate( tbl%iflag_Npole_t(np) )
      allocate( tbl%iflag_ele_Spole(np) )
      allocate( tbl%iflag_ele_Npole(np) )
      tbl%iflag_Spole_t = 0
      tbl%iflag_Npole_t = 0
      tbl%iflag_ele_Spole = 0
      tbl%iflag_ele_Npole = 0
!
      allocate( tbl%iflag_center_t(0:num+1) )
      tbl%iflag_center_t = 0
!
      tbl%nele_around_pole = tbl%nidx_global_fem(3) / 2
!
      end subroutine alloc_nnod_nele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine alloc_iele_sph_mesh(tbl)
!
      type(comm_table_make_sph), intent(inout) :: tbl
!
      integer(kind = kint) :: num, np
!
!
      np =  tbl%ndomain_fem(1)
      num = tbl%nidx_global_fem(1)
      allocate( tbl%irev_sph_r(0:num,np) )
      allocate( tbl%inod_sph_r(0:tbl%nmax_nod_sph_r,np) )
      allocate( tbl%ie_sph_r(tbl%nmax_ele_sph_r,2,np) )
      tbl%irev_sph_r = 0
      tbl%inod_sph_r = 0
      tbl%ie_sph_r =   0
!
      allocate( tbl%ie_center_r(2,np) )
      tbl%ie_center_r = 0
!
      np =  tbl%ndomain_fem(2)
      num = tbl%nidx_global_fem(2)
      allocate( tbl%irev_sph_t(0:num+1,np) )
      allocate( tbl%inod_sph_t(0:tbl%nmax_nod_sph_t+1,np) )
      allocate( tbl%ie_sph_t(tbl%nmax_ele_sph_t,2,np) )
      allocate( tbl%ie_center_t(num-1,2) )
      tbl%irev_sph_t = 0
      tbl%inod_sph_t = 0
      tbl%ie_sph_t =    0
      tbl%ie_center_t = 0
      tbl%ie_center_Sp = 0
      tbl%ie_center_Np = 0
!
      allocate( tbl%ie_Spole_t(2,np) )
      allocate( tbl%ie_Npole_t(2,np) )
      tbl%ie_Spole_t =  0
      tbl%ie_Npole_t =  0
!
      allocate( tbl%inod_sph_ct(0:num+1) )
      allocate( tbl%irev_sph_ct(0:num+1) )
      tbl%inod_sph_ct = 0
      tbl%irev_sph_ct = 0
!
      np =  tbl%ndomain_fem(3)
      num = tbl%nidx_global_fem(3)
      allocate( tbl%ie_sph_p(num,2) )
      tbl%ie_sph_p = 0
!!
      end subroutine alloc_iele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine alloc_1d_comm_tbl_4_sph(ntot_import, ntot_export, tbl)
!
      integer(kind = kint), intent(in) :: ntot_import, ntot_export
      type(comm_table_make_sph), intent(inout) :: tbl
!
!
      allocate(tbl%item_import_rtp(ntot_import))
      allocate(tbl%item_export_rtp(ntot_export))
      allocate(tbl%item_import_1d_rtp(3,ntot_import))
      allocate(tbl%item_export_1d_rtp(3,ntot_export))
      if(ntot_import .gt. 0) tbl%item_import_rtp = 0
      if(ntot_export .gt. 0) tbl%item_export_rtp = 0
      if(ntot_import .gt. 0) tbl%item_import_1d_rtp = 0
      if(ntot_export .gt. 0) tbl%item_export_1d_rtp = 0
!
      end subroutine alloc_1d_comm_tbl_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_nnod_nele_sph_mesh(tbl)
!
      type(comm_table_make_sph), intent(inout) :: tbl
!
!
      deallocate(tbl%inod_sph_ct, tbl%irev_sph_ct)
      deallocate(tbl%ie_sph_r, tbl%ie_sph_t, tbl%ie_sph_p)
      deallocate(tbl%irev_sph_r, tbl%irev_sph_t)
      deallocate(tbl%inod_sph_r, tbl%inod_sph_t)
      deallocate(tbl%iflag_internal_r, tbl%nnod_sph_r, tbl%nele_sph_r)
      deallocate(tbl%iflag_internal_t, tbl%nnod_sph_t, tbl%nele_sph_t)
      deallocate(tbl%iflag_neib_r, tbl%iflag_neib_t)
      deallocate(tbl%iflag_center_r, tbl%iflag_center_t)
      deallocate(tbl%iflag_Spole_t, tbl%iflag_Npole_t)
      deallocate(tbl%iflag_ele_center)
      deallocate(tbl%iflag_ele_Spole, tbl%iflag_ele_Npole)
      deallocate(tbl%iflag_ele_r, tbl%iflag_ele_t)
      deallocate(tbl%ie_center_t)
!
      end subroutine dealloc_nnod_nele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_1d_comm_tbl_4_sph(tbl)
!
      type(comm_table_make_sph), intent(inout) :: tbl
!
!
      deallocate(tbl%item_import_rtp, tbl%item_export_rtp)
      deallocate(tbl%item_import_1d_rtp, tbl%item_export_1d_rtp)
!
      end subroutine dealloc_1d_comm_tbl_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_iele_4_sph_connects(tbl)
!
      integer(kind = kint) :: k, ip, i12(2)
      type(comm_table_make_sph), intent(in) :: tbl
!
!
      write(*,'(a,255i6)') 'iflag_neib_r', tbl%ndomain_fem(1)
      do k = 1, tbl%ndomain_fem(1)
        write(*,'(255i6)') k, tbl%iflag_neib_r(k,1:tbl%ndomain_fem(1))
      end do
      write(*,*) 'iflag_neib_t', tbl%ndomain_fem(2)
      do k = 1, tbl%ndomain_fem(2)
        write(*,'(255i6)') k, tbl%iflag_neib_t(k,1:tbl%ndomain_fem(2))
      end do
!
      write(*,'(a,255i6)') 'iflag_internal_r',                          &
     &                       tbl%nidx_global_fem(1), tbl%ndomain_fem(1)
      write(*,'(a,255i6)') 'Center: ',                                  &
     &                    tbl%iflag_center_r(1:tbl%ndomain_fem(1))
      do k = 0, tbl%nidx_global_fem(2)+1
        write(*,'(255i6)') k, tbl%iflag_center_t(k),                    &
     &                    tbl%inod_sph_ct(k), tbl%irev_sph_ct(k)
      end do
      write(*,'(a)') 'connectivity for center element'
      write(*,'(a,255i6)') 'S-pole: ', tbl%ie_center_Sp
      do k = 1, tbl%nidx_global_fem(2)-1
        write(*,'(255i6)') k, tbl%ie_center_t(k,1:2)
      end do
      write(*,'(a,255i6)') 'N-pole: ', tbl%ie_center_Np
!
      write(*,'(a,255i6)') 'radial numbers: ',                          &
     &         tbl%nnod_sph_r(1:tbl%ndomain_fem(1))
      do k = 1, tbl%nidx_global_fem(1)
        write(*,'(255i6)') k,                                           &
     &                tbl%iflag_internal_r(k,1:tbl%ndomain_fem(1))
      end do
      write(*,*) 'iflag_internal_t',                                    &
     &          tbl%nidx_global_fem(2), tbl%ndomain_fem(2)
      write(*,*) 'numbers: ', tbl%nnod_sph_t
      write(*,'(a,255i6)') 'S_pole: ',                                  &
     &                    tbl%iflag_Spole_t(1:tbl%ndomain_fem(2))
      do k = 1, tbl%nidx_global_fem(2)
        write(*,'(255i6)') k,                                           &
     &                tbl%iflag_internal_t(k,1:tbl%ndomain_fem(2))
      end do
      write(*,'(a,255i6)') 'N_pole: ',                                  &
     &                    tbl%iflag_Npole_t(1:tbl%ndomain_fem(2))
!
      do ip = 1, tbl%ndomain_fem(1)
        write(*,*) 'k, tbl%ie_sph_r(k,1:2,ip) for ',                    &
     &            ip, tbl%nele_sph_r(ip)
        i12(1:2) = tbl%ie_center_r(1:2,ip)
        write(*,*) 'Center: ' , i12(1:2), tbl%inod_sph_r(i12(1:2),ip)
        do k = 1, tbl%nele_sph_r(ip)
          i12(1:2) = tbl%ie_sph_r(k,1:2,ip)
          write(*,*) k, i12(1:2), tbl%inod_sph_r(i12(1:2),ip)
        end do
      end do
!
      do ip = 1, tbl%ndomain_fem(2)
        write(*,*) 'k, tbl%ie_sph_t(k,1:2,ip) for ', ip
        i12(1:2) = tbl%ie_Spole_t(1:2,ip)
        write(*,*) 'S_pole: ', i12(1:2), tbl%inod_sph_t(i12(1:2),ip)
        do k = 1, tbl%nele_sph_t(ip)
          i12(1:2) = tbl%ie_sph_t(k,1:2,ip)
          write(*,*) k, i12(1:2), tbl%inod_sph_t(i12(1:2),ip)
        end do
        i12(1:2) = tbl%ie_Npole_t(1:2,ip)
        write(*,*) 'N_pole: ', i12(1:2), tbl%inod_sph_t(i12(1:2),ip)
      end do
!
      write(*,*) 'k, tbl%ie_sph_p(k,1:2) for all'
      do k = 1, tbl%nidx_global_fem(3)
        write(*,*) k, tbl%ie_sph_p(k,1:2)
      end do
!
      end subroutine check_iele_4_sph_connects
!
! ----------------------------------------------------------------------
!
      end module t_sph_mesh_1d_connect
