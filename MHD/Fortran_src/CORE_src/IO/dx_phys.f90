!
!      module dx_phys
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine write_dx_header(istep, nnod_ele, nnod, nele,          &
!     &          num_field, ncomp_field, field_name,                    &
!     &          id_output, merged_header, dx_node_fname, dx_conn_fname)
!      subroutine write_dx_phys(istep, nsize_nod, nnod, num_field,      &
!     &         ntot_comp, ncomp_field, d_nod, id_output, merged_header)
!
      module dx_phys
!
      use m_precision
!
      implicit none
!
      private :: set_dx_phys_file_name, set_dx_head_file_name
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_dx_header(istep, nnod_ele, nnod, nele,           &
     &          num_field, ncomp_field, field_name,                     &
     &          id_output, merged_header, dx_node_fname, dx_conn_fname)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_ele, nnod, nele
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
!
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), intent(in) ::  id_output
      character(len=kchara), intent(in) :: dx_node_fname
      character(len=kchara), intent(in) :: dx_conn_fname
      character(len=kchara), intent(in) :: merged_header
!
      integer(kind = kint) ::  j, nfield, nfield2, iele
      integer(kind = kint) ::  numele_quad
      character(len=kchara) :: file_name
!
!
      if (nnod_ele .eq. num_t_linear) then
        numele_quad = 5*nele
      else if (nnod_ele .eq. num_t_quad) then
        numele_quad = 20*nele
      else if (nnod_ele .eq. num_t_lag) then
        numele_quad = 40*nele
      else if (nnod_ele .eq. num_triangle) then
        numele_quad = nele
      else if (nnod_ele .eq. num_linear_edge) then
        numele_quad = nele
      end if
!
      do nfield = 1, num_field
        nfield2 = nfield + 2
!
        call set_dx_head_file_name(istep, nfield2, merged_header,       &
     &      file_name)
!
        write(*,*) 'merged grid data:     ', trim(file_name)
        open (id_output,  file=file_name,                               &
     &                   form='formatted', status ='unknown' )
!
        write(id_output,1000) 
        write(id_output,1001) 
        write(id_output,1002) nnod
        write(id_output,1003) trim(dx_node_fname)
!
        write(id_output,1000) 
        write(id_output,1004) 
        write(id_output,1000) 
!
        if (nnod_ele .eq. num_t_linear) then
          write(id_output,1035)  numele_quad
          write(id_output,1006)  trim(dx_conn_fname)
          write(id_output,1037) 
        else if (nnod_ele .eq. num_t_quad                             &
     &    .or. nnod_ele .eq. num_t_lag ) then
          write(id_output,1005)  numele_quad
          write(id_output,1006)  trim(dx_conn_fname)
          write(id_output,1007) 
        else if (nnod_ele .eq. num_triangle) then
          write(id_output,1045)  numele_quad
          write(id_output,1006)  trim(dx_conn_fname)
          write(id_output,1047) 
        else if (nnod_ele .eq. num_linear_edge) then
          write(id_output,1055)  numele_quad
          write(id_output,1006)  trim(dx_conn_fname)
          write(id_output,1057) 
        end if
        write(id_output,1008) 
!
        do  j = 1, num_field
!
          write(id_output,1000) 
          if ( ncomp_field(j) .eq. 1 ) then
            write(id_output,1009) field_name(j)
          else if ( ncomp_field(j) .eq. 3 ) then
            write(id_output,1013) field_name(j)
          else if ( ncomp_field(j) .eq. 6 ) then
            write(id_output,1015) field_name(j)
          end if
          write(id_output,1000) 
!
          if ( ncomp_field(j) .eq. 1 ) then
            write(id_output,1010) j+2, nnod
          else if ( ncomp_field(j) .eq. 3 ) then
            write(id_output,1014) j+2, nnod
          else if ( ncomp_field(j) .eq. 6 ) then
            write(id_output,1016) j+2, nnod
          end if
          call set_dx_phys_file_name(istep,j+2,merged_header,file_name)
          write(id_output,1011) trim(file_name)
          write(id_output,1012) 
!
        end do
!
        write(id_output,1000) 
        write(id_output,1017) 
        write(id_output,1025) 
        do  j = 1, num_field
          write(id_output,1026) j+2, trim(field_name(j))
        end do
        write(id_output,1000) 
        write(id_output,1018) 
!
        write(id_output,1019) 
        write(id_output,1020) 
!
        write(id_output,1021) nfield2
!
        write(id_output,1000) 
        write(id_output,1022) 
!
        close (id_output)
      end do
!
 1000 format('#')
 1001 format('# node information')
 1002 format('object 1 class array type float rank 1 shape 3 items',    &
     &       i16, ' msb ascii')
 1003 format('data file ', a)
 1004 format('# element connectivity')
!
 1005 format('object 2 class array type int rank 1 shape 4 items',      &
     &       i16, ' msb ascii')
 1035 format('object 2 class array type int rank 1 shape 8 items',      &
     &       i16, ' msb ascii')
 1045 format('object 2 class array type int rank 1 shape 3 items',      &
     &       i16, ' msb ascii')
 1055 format('object 2 class array type int rank 1 shape 2 items',      &
     &       i16, ' msb ascii')
!
 1006 format('data file ', a)
!
 1007 format('attribute "element type" string "tetrahedra"')
 1037 format('attribute "element type" string "cubes"')
 1047 format('attribute "element type" string "triangles"')
 1057 format('attribute "element type" string "lines"')
!
 1008 format('attribute "ref" string "positions"')
!
 1009 format('# scaler...', a)
 1010 format('object',i6,' class array type float rank 0 items',        &
     &       i16, ' msb ascii')
 1011 format('data file ', a)
 1012 format('attribute "dep" string "positions"')
!
 1013 format('# vector...', a)
 1014 format('object',i6,' class array type float rank 1 shape 3 items',&
     &       i16, ' msb ascii')
!
 1015 format('# tensor...', a)
 1016 format('object',i6,' class array type float rank 2 shape 3 3 ',   &
     & 'items', i16, ' msb ascii')
!
 1017 format('# the field with "positions", "connections", and "data"')
 1025 format('# Choose as data from following components')
 1026 format('# No.', i6, ': ', a)
 1018 format('object "irregular positions irregular connections ascii', &
     &       ' file" class field')
!
 1019 format('component "positions" value 1')
 1020 format('component "connections" value 2')
 1021 format('component "data" value ', i4)
 1022 format('end')
!
      end subroutine write_dx_header
!
! -----------------------------------------------------------------------
!
      subroutine write_dx_phys(istep, nsize_nod, nnod, num_field,       &
     &         ntot_comp, ncomp_field, d_nod, id_output, merged_header)
!
      integer (kind=kint), intent(in) :: nsize_nod, nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      real(kind = kreal), intent(in) :: d_nod(nsize_nod,ntot_comp)
!
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), intent(in) ::  id_output
      character(len=kchara), intent(in) :: merged_header
!
      integer(kind=kint) :: nd, inod, ii, j
      character(len=kchara) :: file_name
!
!
      if( ntot_comp .ge. 1 ) Then
        ii = 1
        do  j = 1,num_field
!
          call set_dx_phys_file_name(istep,j+2,merged_header,file_name)
!
          write(*,*) 'merged physical data: ', file_name
          open (id_output, file=file_name,                              &
     &           form='formatted', status ='unknown')
!
          if ( ncomp_field(j) .eq. 1 ) then
            do inod = 1, nnod
              write(id_output,'(1pE25.15e3)') d_nod(inod,ii)
            end do
            ii = ii + 1
          else if ( ncomp_field(j) .eq. 3 ) then
            do inod = 1, nnod
              write(id_output,'(1p3E25.15e3)')                          &
     &                (d_nod(inod,nd), nd = ii, ii+2)
            end do
            ii = ii + 3
          else if ( ncomp_field(j) .eq. 6 ) then
            do inod = 1, nnod
              write(id_output,'(1p9E25.15e3)')                          &
     &            (d_nod(inod,nd), nd=ii,ii+2),                         &
     &             d_nod(inod,ii+1), (d_nod(inod,nd), nd=ii+3,ii+4),    &
     &             d_nod(inod,ii+2), d_nod(inod,ii+4), d_nod(inod,ii+5)
            end do
            ii = ii + 6
          end if
!
          close(id_output)
        end do
!
      ENDIF
!
      end subroutine write_dx_phys
!
! -----------------------------------------------------------------------
!
      subroutine set_dx_phys_file_name(istep, field_id, dx_head, fname)
!
      use set_parallel_file_name
!
      integer( kind=kint ), intent(in) :: field_id, istep
      character (len=kchara), intent(in) :: dx_head
      character (len=kchara), intent(inout) :: fname
!
      character (len=kchara) :: fname_tmp1, fname_tmp2
!
!
      call add_int_suffix(field_id, dx_head, fname_tmp1)
      call add_int_suffix(istep, fname_tmp1, fname_tmp2)
!
      call add_fld_extension(fname_tmp2, fname_tmp1)
      call add_dat_extension(fname_tmp1, fname)
!
      end subroutine set_dx_phys_file_name
!
!-----------------------------------------------------------------------
!
      subroutine set_dx_head_file_name(istep, id_fld, dx_head, fname)
!
      use set_parallel_file_name
!
      integer( kind=kint ), intent(in) :: id_fld, istep
      character (len=kchara), intent(in) :: dx_head
      character (len=kchara), intent(inout) :: fname
!
      character (len=kchara) :: fname_tmp1, fname_tmp2
!
!
        call add_int_suffix(istep, dx_head, fname_tmp2)
        call add_int_suffix(id_fld, fname_tmp2, fname_tmp1)
        call add_dx_extension(fname_tmp1, fname)
!
!
      end subroutine set_dx_head_file_name
!
!-----------------------------------------------------------------------
!
      end module dx_phys
