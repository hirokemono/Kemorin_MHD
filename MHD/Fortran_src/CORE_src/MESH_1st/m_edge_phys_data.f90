!m_edge_phys_data.f90
!     module m_edge_phys_data
!.......................................................................
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine allocate_edge_dat_names
!      subroutine allocate_edge_data_arrays
!      subroutine allocate_edge_fld_id_4_rms
!
!      subroutine deallocate_edge_data_arrays
!      subroutine deallocate_edge_fld_id_4_rms
!
!      subroutine check_edge_phys_data(my_rank, numdir, i_field)
!
!
      module m_edge_phys_data
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_edge_phys
!    number of physical data
      integer (kind=kint) :: ntot_edge_phys
!
      integer (kind=kint), allocatable, target :: num_edge_component(:)
      integer (kind=kint), allocatable, target                          &
     &                    :: istack_edge_component(:)
      character(len=kchara), allocatable, target :: phys_edge_name(:)
      integer (kind=kint), allocatable, target :: iorder_edge_phys(:)
!
      integer (kind=kint), allocatable, target :: iflag_edge_update(:)
      real(kind=kreal), allocatable, target :: d_edge(:,:)
!
      integer (kind=kint) :: num_edge_phys_vis
      integer (kind=kint) :: num_tot_edge_phys_vis
!
      integer (kind=kint), allocatable, target :: iflag_rms_edge_fld(:)
!
      integer (kind=kint) :: num_edge_phys_4_rms
      integer (kind=kint) :: ntot_comp_edge_phys_4_rms
      integer (kind=kint), allocatable:: ifield_rms_edge(:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_edge_dat_names
!
      allocate( phys_edge_name(num_edge_phys) )
      allocate( num_edge_component(num_edge_phys) )
      allocate( istack_edge_component(0:num_edge_phys) )
      allocate( iorder_edge_phys(num_edge_phys) )
      allocate( iflag_rms_edge_fld(num_edge_phys) )
!
      phys_edge_name = ''
      num_edge_component =    0
      istack_edge_component = 0
      iorder_edge_phys =      1
      iflag_rms_edge_fld =    0
!
      end subroutine allocate_edge_dat_names
!
!  --------------------------------------------------------------------
!
      subroutine allocate_edge_data_arrays
!
      use m_geometry_parameter
!
      allocate( d_edge(numedge,ntot_edge_phys) )
      allocate( iflag_edge_update(ntot_edge_phys) )
      iflag_edge_update = 0
      d_edge = 0.0d0
!
      end subroutine allocate_edge_data_arrays
!
! --------------------------------------------------------------------
!
       subroutine allocate_edge_fld_id_4_rms
!
       allocate (ifield_rms_edge(ntot_comp_edge_phys_4_rms))
       if(ntot_comp_edge_phys_4_rms .gt. 0) ifield_rms_edge = 0
!
       end subroutine allocate_edge_fld_id_4_rms
!
!  --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine deallocate_edge_data_arrays
!
      deallocate( iorder_edge_phys, iflag_rms_edge_fld)
      deallocate( num_edge_component )
      deallocate( istack_edge_component )
      deallocate( iorder_edge_phys, iflag_rms_edge_fld)
      deallocate( d_edge, iflag_edge_update )
!
      end subroutine deallocate_edge_data_arrays
!
! --------------------------------------------------------------------
!
       subroutine deallocate_edge_fld_id_4_rms
!
       deallocate (ifield_rms_edge)
!
       end subroutine deallocate_edge_fld_id_4_rms
!
!  --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine check_edge_phys_data(my_rank, numdir, i_field)
!
      use m_geometry_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numdir, i_field
      integer(kind = kint) :: iedge, nd
!
      write(50+my_rank,*) 'iedge, edge field: ', i_field, numdir
      do iedge = 1, numedge
        write(50+my_rank,'(i10,1p10e25.14)')                            &
     &         iedge, (d_edge(iedge,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_edge_phys_data
!
!  --------------------------------------------------------------------
!
      end module m_edge_phys_data
