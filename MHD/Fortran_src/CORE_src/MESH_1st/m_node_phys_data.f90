!m_node_phys_data.f90
!     module m_node_phys_data
!
!> @brief nodal field data for FEM
!
!     Written by H. Matsui
!
!       subroutine allocate_phys_name
!       subroutine allocate_data_arrays
!
!       subroutine deallocate_phys_name
!       subroutine deallocate_data_arrays
!
!
!      subroutine check_nodal_field_name
!      subroutine check_nodal_data(my_rank, numdir, i_field)
!
!      subroutine link_nodal_fld_type_names(nod_fld)
!      subroutine link_nodal_fld_type(nod_fld)
!
      module m_node_phys_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
!>       Structure for nodal field data
      type(phys_data), save :: nod_fld1
!nod_fld1%ntot_phys
!
!      integer (kind=kint) :: num_nod_phys
!    number of physical data
!      integer (kind=kint) :: num_tot_nod_phys
!
!      integer (kind=kint), pointer :: num_nod_component(:)
! 
!      integer (kind=kint), pointer                          &
!     &                    :: istack_nod_component(:)
! 
!      integer (kind=kint), pointer :: iorder_nod_phys(:)
!
!      character (len=kchara), pointer :: phys_nod_name(:)
! 
      real (kind=kreal), pointer :: d_nod(:,:)
! 
!      integer (kind=kint), pointer :: iflag_nod_update(:)
!
!     paraamaters to visualizer
!
!      integer (kind=kint) :: num_nod_phys_vis
!    number of physical data to visualizer
!      integer (kind=kint) :: num_tot_nod_phys_vis
!
!     paramaters for monitoring
!
!      integer(kind=kint), pointer                           &
!     &                   :: iflag_nod_fld_monitor(:)
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
       subroutine allocate_phys_name
!
!
       allocate( nod_fld1%phys_name(nod_fld1%num_phys) )
       allocate( nod_fld1%num_component(nod_fld1%num_phys) )
       allocate( nod_fld1%istack_component(0:nod_fld1%num_phys) )
       allocate( nod_fld1%iorder_eletype(nod_fld1%num_phys) )
       allocate( nod_fld1%iflag_monitor(nod_fld1%num_phys) )
!
       nod_fld1%phys_name = ''
       nod_fld1%num_component =    0
       nod_fld1%istack_component = 0
       nod_fld1%iflag_monitor   =  0
       nod_fld1%iorder_eletype =   1
!
       end subroutine allocate_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine allocate_data_arrays
!
       use m_geometry_data
!
       allocate( nod_fld1%iflag_update(nod_fld1%ntot_phys) )
       allocate( d_nod(node1%numnod,nod_fld1%ntot_phys) )
!
       nod_fld1%iflag_update = 0
       d_nod = 0.0d0
!
       end subroutine allocate_data_arrays
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
       subroutine deallocate_phys_name
!
       deallocate( nod_fld1%phys_name )
       deallocate( nod_fld1%num_component )
       deallocate( nod_fld1%istack_component )
       deallocate( nod_fld1%iorder_eletype )
       deallocate( nod_fld1%iflag_monitor )
!
       end subroutine deallocate_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_data_arrays
!
       deallocate( nod_fld1%iflag_update )
       deallocate( d_nod )
!
       end subroutine deallocate_data_arrays
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_nodal_field_name
!
!
      integer(kind = kint) :: i
!
      write(*,*) 'num_nod_phys ',nod_fld1%num_phys
      write(*,*) 'num_nod_phys_vis ',nod_fld1%num_phys_viz
      write(*,*) 'id#, num_component, stack_component, field_name '
      do i = 1, nod_fld1%num_phys
        write(*,'(3i6,2x,a2,a)') i, nod_fld1%num_component(i),          &
     &      nod_fld1%istack_component(i), '  ',                         &
     &      trim(nod_fld1%phys_name(i))
      end do
!
      end subroutine check_nodal_field_name
!
!   ---------------------------------------------------------------------
!
      subroutine check_nodal_data(my_rank, numdir, i_field)
!
       use m_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numdir, i_field
      integer(kind = kint) :: inod, nd
!
      write(50+my_rank,*) 'inod, nodal field: ', i_field, numdir
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         inod, (d_nod(inod,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_nodal_data
!
!  --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_nodal_fld_type_names(nod_fld)
!
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      nod_fld%num_phys =  nod_fld1%num_phys
      nod_fld%ntot_phys = nod_fld1%ntot_phys
!
      nod_fld%num_phys_viz =  nod_fld1%num_phys_viz
      nod_fld%ntot_phys_viz = nod_fld1%ntot_phys_viz
!
      nod_fld%num_component =>    nod_fld1%num_component
      nod_fld%istack_component => nod_fld1%istack_component
      nod_fld%iorder_eletype =>   nod_fld1%iorder_eletype
      nod_fld%iflag_monitor =>    nod_fld1%iflag_monitor
      nod_fld%phys_name =>        nod_fld1%phys_name
!
      end subroutine link_nodal_fld_type_names
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine link_nodal_fld_type(nod_fld)
!
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call link_nodal_fld_type_names(nod_fld)
!
      nod_fld%d_fld => d_nod
!
      end subroutine link_nodal_fld_type
!
! -------------------------------------------------------------------
!
      end module m_node_phys_data
