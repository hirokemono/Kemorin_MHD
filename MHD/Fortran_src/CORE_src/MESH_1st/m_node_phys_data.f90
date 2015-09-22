!m_node_phys_data.f90
!     module m_node_phys_data
!
!> @brief nodal field data for FEM
!
!     Written by H. Matsui
!
!       subroutine allocate_data_arrays
!
!       subroutine deallocate_phys_name
!       subroutine deallocate_data_arrays
!      subroutine check_nodal_data(my_rank, numdir, i_field)
!
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
!nod_fld1%d_fld
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
!      real (kind=kreal), pointer :: d_nod(:,:)
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
       subroutine allocate_data_arrays
!
       use m_geometry_data
!
       nod_fld1%n_point = node1%numnod
       allocate( nod_fld1%iflag_update(nod_fld1%ntot_phys) )
       allocate( nod_fld1%d_fld(nod_fld1%n_point,nod_fld1%ntot_phys) )
!
       nod_fld1%iflag_update = 0
       nod_fld1%d_fld = 0.0d0
!
       end subroutine allocate_data_arrays
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
       subroutine deallocate_phys_name
!
       call dealloc_phys_name_type(nod_fld1)
!
       end subroutine deallocate_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_data_arrays
!
       deallocate( nod_fld1%iflag_update )
       deallocate( nod_fld1%d_fld )
!
       end subroutine deallocate_data_arrays
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
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
     &         inod, (nod_fld1%d_fld(inod,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_nodal_data
!
!  --------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine link_nodal_fld_type(nod_fld)
!
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call link_field_name_type(nod_fld1, nod_fld)
      nod_fld%n_point = nod_fld1%n_point
      nod_fld%d_fld =>   nod_fld1%d_fld
!
      end subroutine link_nodal_fld_type
!
! -------------------------------------------------------------------
!
      end module m_node_phys_data
