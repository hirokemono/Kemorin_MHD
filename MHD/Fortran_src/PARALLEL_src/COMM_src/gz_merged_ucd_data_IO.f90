!
!      module gz_merged_ucd_data_IO
!
!      Written by H. Matsui on Feb., 2007
!
!!      subroutine write_merged_gz_ucd_fields(ucd, m_ucd)
!!      subroutine write_merged_gz_ucd_mesh(ucd, m_ucd)
!!        type(ucd_data), intent(in) :: ucd
!!        type(merged_ucd_data), intent(in) :: m_ucd
!
      module gz_merged_ucd_data_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use gz_ucd_data_IO
!
      implicit none
!
      private :: write_merged_gz_ucd_connect
      private :: write_merged_gz_udt_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_gz_ucd_fields(ucd, m_ucd)
!
      use t_ucd_data
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
!
      if(my_rank .eq. 0) then
        call write_gz_udt_field_header(ucd%num_field, ucd%num_comp,     &
     &      ucd%phys_name)
      end if
!
      call write_merged_gz_udt_field                                    &
     &   (ucd%nnod, ucd%ntot_comp, ucd%d_ucd,                           &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod)
!
      end subroutine write_merged_gz_ucd_fields
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_gz_ucd_mesh(ucd, m_ucd)
!
      use m_phys_constants
      use t_ucd_data
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
!
      if(my_rank .eq. 0) then
        call write_gz_udt_mesh_header                                   &
     &     (m_ucd%istack_merged_intnod(nprocs),                         &
     &      m_ucd%istack_merged_ele(nprocs), ucd%ntot_comp)
      end if
!
      call write_merged_gz_udt_field(ucd%nnod, n_vector, ucd%xx,        &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod)
      call write_merged_gz_ucd_connect(ucd%nele, ucd%nnod_4_ele,        &
     &    ucd%ie, m_ucd%istack_merged_ele)
!
      end subroutine write_merged_gz_ucd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_merged_gz_ucd_connect(numele, nnod_ele, ie,      &
     &          istack_numele)
!
      use m_geometry_constants
      use m_merged_ucd_data
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: numele
      integer(kind = kint_gl), intent(in) :: ie(numele,nnod_ele)
!
      integer(kind = kint_gl), intent(in) :: istack_numele(0:nprocs)
!
      integer(kind = kint) :: ip, num4, isend_rank
      integer(kind = kint_gl) :: nele, iele
!
!
      if(my_rank .eq. 0) then
        nele = istack_numele(1)
        do iele = 1, nele
          iele_single_ucd(iele) = iele
        end do
!
        call write_gz_ucd_mesh_connect(nele, nnod_ele,                  &
     &      nele, iele_single_ucd(1), ie(1,1) )
      end if
!
      do ip = 2, nprocs
        isend_rank = ip- 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank ) then
          num4 = int(numele*nnod_ele)
          call MPI_ISEND(ie(1,1), num4, CALYPSO_INTEGER,                &
     &        izero, 0, CALYPSO_COMM, req1, ierr_MPI)
        end if
!
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num4 = int(istack_numele(ip) - istack_numele(ip-1))           &
     &          * nnod_ele
          call MPI_IRECV(ie_single_ucd(1), num4, CALYPSO_INTEGER,       &
     &        (ip-1), 0, CALYPSO_COMM, req2, ierr_MPI)
!
          call MPI_WAITALL (ione, req2, sta2, ierr_MPI)
!
          nele = istack_numele(ip) - istack_numele(ip-1)
          do iele = 1, nele
            iele_single_ucd(iele) = iele + istack_numele(ip-1)
          end do
!
          call write_gz_ucd_mesh_connect(nele, nnod_ele, nele,          &
     &        iele_single_ucd(1), ie_single_ucd(1) )
        end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr_MPI)
        end if
      end do 
      call  calypso_MPI_barrier
!
      end subroutine write_merged_gz_ucd_connect
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_gz_udt_field(numnod, ncomp_field, d_nod,  &
     &          istack_numnod, istack_intnod)
!
      use m_merged_ucd_data
!
      integer(kind = kint_gl), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ncomp_field
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_field)
!
      integer(kind=kint_gl), intent(in) :: istack_numnod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: istack_intnod(0:nprocs)
!
!
      integer(kind = kint) :: ip, num4, isend_rank
      integer(kind = kint_gl) :: nnod, inod
!
!
      if(my_rank .eq. 0) then
        nnod = istack_intnod(1)
        do inod = 1, nnod
          inod_single_ucd(inod) = inod
        end do
!
        call write_gz_ucd_field_data(numnod, ncomp_field,               &
     &      nnod, inod_single_ucd(1), d_nod(1,1) )
      end if
!
      do ip = 2, nprocs
        isend_rank = ip - 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank) then
          num4 = int(numnod*ncomp_field)
          call MPI_ISEND(d_nod(1,1), num4, CALYPSO_REAL,                &
     &      izero, 0, CALYPSO_COMM, req1, ierr_MPI)
        end if
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num4 = int(istack_numnod(ip) - istack_numnod(ip-1))           &
     &         * ncomp_field
          call MPI_IRECV(d_single_ucd(1), num4, CALYPSO_REAL,           &
     &        (ip-1), 0, CALYPSO_COMM, req2, ierr_MPI)
!
          call MPI_WAITALL (ione, req2, sta2, ierr_MPI)
!
          nnod = istack_numnod(ip) - istack_numnod(ip-1)
          do inod = 1, nnod
            inod_single_ucd(inod) = inod + istack_intnod(ip-1)
          end do
!
          call write_gz_ucd_field_data(nnod, ncomp_field,               &
     &        nnod, inod_single_ucd(1), d_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr_MPI)
        end if
!
      end do 
      call  calypso_MPI_barrier
!
      end subroutine write_merged_gz_udt_field
!
! -----------------------------------------------------------------------
!
      end module gz_merged_ucd_data_IO
