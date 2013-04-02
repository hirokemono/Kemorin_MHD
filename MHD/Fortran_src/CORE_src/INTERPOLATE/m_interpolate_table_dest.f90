!
!      module m_interpolate_table_dest
!
!> @brief Interpolation table for target grid
!
!     Written by H. Matsui on Aug., 2006
!
!
!      subroutine allocate_itp_num_dest(num_org_pe)
!      subroutine allocate_itp_table_dest
!      subroutine allocate_itp_coef_dest
!      subroutine allocate_itp_work_dest(num_org_pe)
!
!      subroutine deallocate_itp_num_dest
!      subroutine deallocate_itp_table_dest
!      subroutine deallocate_itp_coef_dest
!      subroutine deallocate_itp_work_dest
!
!      subroutine check_table_in_org_1(id_file)
!      subroutine check_table_in_org_2(id_file)
!
!
      module m_interpolate_table_dest
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: num_org_domain
!<   number of subdomain to receive interpolated data
      integer(kind = kint) :: iflag_self_itp_recv
!<   flag if original nodes have same prosess
      integer(kind = kint), allocatable :: id_org_domain(:)
!<   subdomain rank to receive interpolated data
      integer(kind = kint), allocatable :: istack_nod_table_dest(:)
!<   end address to receive interpolated data
!
      integer(kind = kint), allocatable                                 &
     &            :: istack_nod_table_wtype_dest(:)
!<   end address to receive interpolated data including interpolate type
!
      integer(kind = kint) :: ntot_table_dest
!<   total number of interpolated node in target subdomain
      integer(kind = kint), allocatable :: inod_dest_4_dest(:)
!<   local node ID to set interpolated data
!
!
      integer(kind = kint), allocatable :: inod_gl_dest(:)
!<   global node ID for target domain
      integer(kind = kint), allocatable :: iele_org_4_dest(:)
!<   local element ID to make interpolation
      integer(kind = kint), allocatable :: itype_inter_dest(:)
!<   interpolation type ID
      real(kind = kreal), allocatable :: coef_inter_dest(:,:)
!<   Coordinate of target node in element coordinate
!
      integer(kind = kint), allocatable :: numnod_dest(:)
!<   number of node to be interpolated in each original domain
      integer(kind = kint), allocatable :: nnod_table_wtype_dest(:)
!<   number of node to be interpolated in each original domain and type
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_num_dest(num_org_pe)
!
      integer(kind = kint), intent(in) :: num_org_pe
!
!
      allocate( id_org_domain(num_org_pe) )
      allocate( istack_nod_table_dest(0:num_org_pe) )
      allocate( istack_nod_table_wtype_dest(0:4*num_org_pe) )
!
      id_org_domain = 0
      istack_nod_table_dest = -1
      istack_nod_table_wtype_dest = -1
!
      end subroutine allocate_itp_num_dest
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_table_dest
!
      allocate( inod_dest_4_dest(ntot_table_dest) )
      inod_dest_4_dest = 0
!
      end subroutine allocate_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_coef_dest
!
      allocate( inod_gl_dest(ntot_table_dest) )
      allocate( iele_org_4_dest(ntot_table_dest) )
      allocate( itype_inter_dest(ntot_table_dest) )
      allocate( coef_inter_dest(ntot_table_dest,3) )
!
      inod_gl_dest = 0
      iele_org_4_dest = 0
      itype_inter_dest = -1
      coef_inter_dest = 0.0d0
!
      end subroutine allocate_itp_coef_dest
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_work_dest(num_org_pe)
!
      integer(kind = kint), intent(in) :: num_org_pe
!
!
      allocate( numnod_dest(0:num_org_pe) )
      allocate( nnod_table_wtype_dest(4*num_org_pe) )
!
      numnod_dest = 0
      nnod_table_wtype_dest = 0
!
      end subroutine allocate_itp_work_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_num_dest
!
      deallocate( id_org_domain )
      deallocate( istack_nod_table_dest)
      deallocate( istack_nod_table_wtype_dest)
!
      end subroutine deallocate_itp_num_dest
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_table_dest
!
      deallocate( inod_dest_4_dest )
!
      end subroutine deallocate_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_coef_dest
!
      deallocate( inod_gl_dest)
      deallocate( iele_org_4_dest)
      deallocate( itype_inter_dest)
      deallocate( coef_inter_dest)
!
      end subroutine deallocate_itp_coef_dest
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_work_dest
!
      deallocate( numnod_dest )
      deallocate( nnod_table_wtype_dest )
!
      end subroutine deallocate_itp_work_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_table_in_org_2(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod
!
      write(id_file,*) '#'
      write(id_file,*) '#  number of domain of target'
      write(id_file,*) '#   domain IDs'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') num_org_domain
      write(id_file,'(10i10)') id_org_domain(1:num_org_domain)
!
      write(id_file,*) '#'
      write(id_file,*) '#  node, domain for original, belonged element'
      write(id_file,*) '#   coefficients'
      write(id_file,*) '#'
!
      write(id_file,'(10i10)') istack_nod_table_dest(1:num_org_domain)
      do inod = 1, ntot_table_dest
        write(id_file,'(2i10,1p3e23.12)') inod_dest_4_dest(inod),       &
     &        iele_org_4_dest(inod), coef_inter_dest(inod,1:3)
      end do
!
      end subroutine check_table_in_org_2
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_dest
