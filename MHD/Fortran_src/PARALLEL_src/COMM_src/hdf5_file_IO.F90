!
!      module hdf5_file_IO
!
!      Written by E. Heien in June 2013
!
!      subroutine parallel_init_hdf5
!      subroutine parallel_finalize_hdf5
!
!      subroutine parallel_write_hdf5_mesh_file
!      subroutine parallel_write_hdf5_field_file(cur_step)
!
!      subroutine parallel_write_xdmf_snap_file(cur_vis_step)
!      subroutine parallel_write_xdmf_evo_file(cur_vis_step)
!
      module hdf5_file_IO
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_ucd_data
      use m_merged_ucd_data
      use m_phys_constants
      use set_ucd_data
      use set_ucd_file_names
      use set_parallel_file_name
!
#ifdef HDF5_IO
      use hdf5
#endif
!
      implicit none
!
      integer(kind = kint) :: ncomp_hdf5
      real(kind=kreal), allocatable :: fld_hdf5(:)
      integer(kind=kint), allocatable :: ie_hdf5(:,:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine parallel_init_hdf5
!
      integer :: hdferr
      integer(kind = kint) :: nnod
!
! Initialize Fortran interface
!
      nnod = internod_ucd_list(my_rank+1)
      allocate( fld_hdf5(9*nnod) )
      allocate( ie_hdf5(8,nele_ucd) )
!
#ifdef HDF5_IO
      call h5open_f(hdferr)
#endif
!
      end subroutine parallel_init_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine parallel_finalize_hdf5
!
      integer :: hdferr
!
! Close Fortran interface
!
#ifdef HDF5_IO
      call h5close_f(hdferr)
#endif
!
      deallocate(fld_hdf5)
!
      end subroutine parallel_finalize_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ie_hdf5
!
      deallocate(ie_hdf5)
!
      end subroutine deallocate_ie_hdf5
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_ele_connect_for_hdf5
!
      integer(kind = kint) :: iele, k1
!
!
!$omp parallel do
      do iele = 1, nele_ucd
        do k1 = 1, nnod_4_ele_ucd
          ie_hdf5(k1,iele) = ie_ucd(iele,k1) - 1
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ele_connect_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_position_for_hdf5
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, internod_ucd_list(my_rank+1)
        fld_hdf5(3*inod-2) = xx_ucd(inod,1)
        fld_hdf5(3*inod-1) = xx_ucd(inod,2)
        fld_hdf5(3*inod  ) = xx_ucd(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_position_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_scalar_field_for_hdf5(ist_fld)
!
      integer(kind = kint), intent(in) :: ist_fld
      integer(kind = kint) :: inod
!
!
      ncomp_hdf5 = 1
!$omp parallel do
      do inod = 1, internod_ucd_list(my_rank+1)
        fld_hdf5(inod) = d_nod_ucd(inod,ist_fld+1)
      end do
!$omp end parallel do
!
      end subroutine copy_scalar_field_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_vector_field_for_hdf5(ist_fld)
!
      integer(kind = kint), intent(in) :: ist_fld
      integer(kind = kint) :: inod
!
!
      ncomp_hdf5 = 3
!$omp parallel do
      do inod = 1, internod_ucd_list(my_rank+1)
        fld_hdf5(3*inod-2) = d_nod_ucd(inod,ist_fld+1)
        fld_hdf5(3*inod-1) = d_nod_ucd(inod,ist_fld+2)
        fld_hdf5(3*inod  ) = d_nod_ucd(inod,ist_fld+3)
      end do
!$omp end parallel do
!
      end subroutine copy_vector_field_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_sym_tensor_field_for_hdf5(ist_fld)
!
      integer(kind = kint), intent(in) :: ist_fld
      integer(kind = kint) :: inod
!
!
      ncomp_hdf5 = 9
!$omp parallel do
      do inod = 1, internod_ucd_list(my_rank+1)
        fld_hdf5(9*inod-8) = d_nod_ucd(inod,ist_fld+1)
        fld_hdf5(9*inod-7) = d_nod_ucd(inod,ist_fld+2)
        fld_hdf5(9*inod-6) = d_nod_ucd(inod,ist_fld+3)
        fld_hdf5(9*inod-5) = d_nod_ucd(inod,ist_fld+1)
        fld_hdf5(9*inod-4) = d_nod_ucd(inod,ist_fld+4)
        fld_hdf5(9*inod-3) = d_nod_ucd(inod,ist_fld+5)
        fld_hdf5(9*inod-2) = d_nod_ucd(inod,ist_fld+3)
        fld_hdf5(9*inod-1) = d_nod_ucd(inod,ist_fld+5)
        fld_hdf5(9*inod  ) = d_nod_ucd(inod,ist_fld+6)
      end do
!$omp end parallel do
!
      end subroutine copy_sym_tensor_field_for_hdf5
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine parallel_write_hdf5_mesh_file
!
#ifdef HDF5_IO
      character(len=kchara), parameter                                  &
     &                      :: node_dataset_name = "nodes"
      character(len=kchara), parameter                                  &
     &                      :: elem_dataset_name = "elements"
!
      character(len = kchara) :: file_name
      integer(hid_t) :: id_hdf5
      integer(hid_t) :: plist_id      ! Property list identifier
      integer :: info
      integer :: dataspace_dims
      integer(hsize_t) :: node_dataspace_dim(2), elem_dataspace_dim(2)
      integer(hid_t) :: node_dataspace_id, elem_dataspace_id
      integer(hid_t) :: node_dataset_id, elem_dataset_id
      integer(hsize_t) :: hyperslab_size(2), hyperslab_offset(2)
      integer(hid_t) :: node_memory_dataspace, elem_memory_dataspace
      integer(hid_t) :: node_file_dataspace, elem_file_dataspace
      integer(hsize_t) :: buf_dims(2)
      integer :: hdferr
!
!
      call set_merged_hdf_mesh_file_name(ucd_header_name, file_name)
!
! Remove our own counts from the offset
!
      info = MPI_INFO_NULL
!
! Setup file access property list with parallel I/O access.
!
      call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
      call h5pset_fapl_mpio_f(plist_id, SOLVER_COMM, info, hdferr)
!
! Create new file collectively.
!
      call h5fcreate_f(file_name, H5F_ACC_TRUNC_F, id_hdf5,             &
          hdferr, access_prp = plist_id)
      call h5pclose_f(plist_id, hdferr)
!
!
! We first have to transpose the data so it fits correctly to the XDMF format
! We also take this opportunity to use the "real" (C-like) indexing
!
        call copy_node_position_for_hdf5
        call copy_ele_connect_for_hdf5
!
! Node data set dimensions are (number of nodes) x 3
!
        dataspace_dims = 2
        node_dataspace_dim(1) = 3
        node_dataspace_dim(2) = istack_internod_ucd_list(nprocs)
        call h5screate_simple_f(dataspace_dims, node_dataspace_dim,     &
            node_dataspace_id, hdferr)
!
! Element data set dimensions are (number of elements) x (nodes per element)
!
        dataspace_dims = 2
        elem_dataspace_dim(1) = nnod_4_ele_ucd
        elem_dataspace_dim(2) = istack_ele_ucd_list(nprocs)
        call h5screate_simple_f(dataspace_dims, elem_dataspace_dim,     &
            elem_dataspace_id, hdferr)
!
! Create the datasets in the file
!
        call h5dcreate_f(id_hdf5, node_dataset_name, H5T_NATIVE_DOUBLE, &
            node_dataspace_id, node_dataset_id, hdferr)
        call h5dcreate_f(id_hdf5, elem_dataset_name,                    &
            H5T_NATIVE_INTEGER, elem_dataspace_id, elem_dataset_id,     &
     &      hdferr)
!
! We can close the created dataspaces now
!
        call h5sclose_f(node_dataspace_id, hdferr)
        call h5sclose_f(elem_dataspace_id, hdferr)
!
! Create dataspace for memory hyperslab for nodes
!
        hyperslab_size(1) = 3
        hyperslab_size(2) = internod_ucd_list(my_rank+1)
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = istack_internod_ucd_list(my_rank)
        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            node_memory_dataspace, hdferr)
!
! Create dataspace for file hyperslab for nodes
!
        call h5dget_space_f(node_dataset_id, node_file_dataspace,       &
     &      hdferr)
        call h5sselect_hyperslab_f(node_file_dataspace,                 &
     &      H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! And similarly create file/memory hyperslabs for elements
!
        hyperslab_size(1) = nnod_4_ele_ucd
        hyperslab_size(2) = nele_ucd
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = istack_ele_ucd_list(my_rank)

        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            elem_memory_dataspace, hdferr)
!
        call h5dget_space_f(elem_dataset_id, elem_file_dataspace,       &
     &      hdferr)
        call h5sselect_hyperslab_f(elem_file_dataspace,                 &
            H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! Create a property list for collective write
!
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, hdferr)
!
! Write the node data (finally!)
!
        buf_dims(1) = 3
        buf_dims(2) = internod_ucd_list(my_rank+1)
        call h5dwrite_f(node_dataset_id, H5T_NATIVE_DOUBLE,             &
            fld_hdf5(1), buf_dims, hdferr, node_memory_dataspace,       &
            node_file_dataspace, plist_id)
!
! And the element data
!
        buf_dims(1) = nnod_4_ele_ucd
        buf_dims(2) = nele_ucd
        call h5dwrite_f(elem_dataset_id, H5T_NATIVE_INTEGER,            &
            ie_hdf5, buf_dims, hdferr, elem_memory_dataspace,           &
            elem_file_dataspace, plist_id)
!
! Close parallel access property list
!
        call h5pclose_f(plist_id, hdferr)
!
! Close the memory and file dataspaces
!
        call h5sclose_f(node_memory_dataspace, hdferr)
        call h5sclose_f(elem_memory_dataspace, hdferr)
        call h5sclose_f(node_file_dataspace, hdferr)
        call h5sclose_f(elem_file_dataspace, hdferr)
!
! Finally close the datasets
!
        call h5dclose_f(node_dataset_id, hdferr)
        call h5dclose_f(elem_dataset_id, hdferr)
!
! Deallocate transpose arrays
!
        call deallocate_ie_hdf5
!
! Close file
!
      call h5fclose_f(id_hdf5, hdferr)
#endif
!
      end subroutine parallel_write_hdf5_mesh_file
!
! -----------------------------------------------------------------------
!
      subroutine parallel_write_hdf5_field_file(cur_step)
!
      integer(kind=kint), intent(in) :: cur_step
#ifdef HDF5_IO
      integer(kind = kint) :: istep, icou
      character(len = kchara) :: file_name
      integer(hid_t) :: id_hdf5
      integer(hid_t) :: plist_id      ! Property list identifier
      integer(hid_t) :: field_dataspace_id
      integer(hid_t) :: field_dataset_id
      integer(hid_t) :: field_file_dataspace, field_memory_dataspace
      integer :: info
      integer(hsize_t) :: field_dataspace_dim(2)
      integer :: dataspace_dims
      integer :: hdferr
      integer(hsize_t) :: buf_dims(2)
      integer(hsize_t) :: hyperslab_size(2), hyperslab_offset(2)
!
! Setup the filename
!
      call set_merged_hdf_field_file_name(ucd_header_name,              &
     &    cur_step, file_name)
!
! Progress update
!
      if (my_rank .eq. 0) write(*,*) 'merged HDF5 field data: ',        &
     &                   trim(file_name)
! Remove our own counts from the offset
!
      info = MPI_INFO_NULL
!
! Setup file access property list with parallel I/O access.
!
      call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
      call h5pset_fapl_mpio_f(plist_id, SOLVER_COMM, info, hdferr)
!
! Create new file collectively.
!
      call h5fcreate_f(file_name, H5F_ACC_TRUNC_F, id_hdf5,             &
        hdferr, access_prp = plist_id)
      call h5pclose_f(plist_id, hdferr)
!
! Go through each of the fields
!
      icou = 0
      do istep = 1, num_field_ucd, 1
!
! Transpose the field data to fit XDMF standards
!
        if(num_comp_ucd(istep) .eq. n_scalar) then
          call copy_scalar_field_for_hdf5(icou)
        else if(num_comp_ucd(istep) .eq. n_vector) then
          call copy_vector_field_for_hdf5(icou)
        else if(num_comp_ucd(istep) .eq. 6) then
          call copy_sym_tensor_field_for_hdf5(icou)
        end if
        icou = icou + num_comp_ucd(istep)
!
! Create a dataspace of the appropriate size
!
        dataspace_dims = 2
        field_dataspace_dim(1) = ncomp_hdf5
        field_dataspace_dim(2) = istack_internod_ucd_list(nprocs)
        call h5screate_simple_f(dataspace_dims, field_dataspace_dim,    &
            field_dataspace_id, hdferr)

!
! Create the dataset for the field
!
        call h5dcreate_f(id_hdf5, trim(phys_name_ucd(istep)),           &
     &      H5T_NATIVE_DOUBLE,  &
            field_dataspace_id, field_dataset_id, hdferr)
!
! Now we can close the dataspace
!
        call h5sclose_f(field_dataspace_id, hdferr)
!
! Create dataspace for memory hyperslab
!
        hyperslab_size(1) = ncomp_hdf5
        hyperslab_size(2) = internod_ucd_list(my_rank+1)
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = istack_internod_ucd_list(my_rank)
        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            field_memory_dataspace, hdferr)
!
! Create dataspace for file hyperslab
!
        call h5dget_space_f(field_dataset_id, field_file_dataspace,     &
     &      hdferr)
        call h5sselect_hyperslab_f(field_file_dataspace,                &
            H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! Create a property list for collective write
!
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F,       &
     &      hdferr)
!
! Write the field data
!
        buf_dims(1) = ncomp_hdf5
        buf_dims(2) = internod_ucd_list(my_rank+1)
        call h5dwrite_f(field_dataset_id, H5T_NATIVE_DOUBLE,            &
            fld_hdf5(1), buf_dims, hdferr, field_memory_dataspace,      &
            field_file_dataspace, plist_id)
!
! Close parallel access property list
!
        call h5pclose_f(plist_id, hdferr)
!
! Close the memory and file dataspaces
!
        call h5sclose_f(field_memory_dataspace, hdferr)
        call h5sclose_f(field_file_dataspace, hdferr)
!
! Close the field dataset
!
        call h5dclose_f(field_dataset_id, hdferr)
      end do
!
! Close file
!
      call h5fclose_f(id_hdf5, hdferr)
#endif
!
      end subroutine parallel_write_hdf5_field_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_snap_file(cur_vis_step)
!
      use m_t_step_parameter
!
      integer(kind=kint), intent(in) :: cur_vis_step
      character(len = kchara) :: xdmf_dir_file
!
! Only write the file on process 0
      if (my_rank .ne. 0) return
!
! Get the XDMF file location/name
      call set_merged_snap_xdmf_file_name(ucd_header_name,              &
     &    cur_vis_step, xdmf_dir_file)
! Open the XDMF file to append
      call parallel_write_xdmf_file(xdmf_dir_file, cur_vis_step)
!
      end subroutine parallel_write_xdmf_snap_file
!
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_evo_file(cur_vis_step)
!
      use m_t_step_parameter
!
      integer(kind=kint), intent(in) :: cur_vis_step
      character(len = kchara) :: xdmf_dir_file
!
! Only write the file on process 0
      if (my_rank .ne. 0) return
!
! Get the XDMF file location/name
      call set_merged_xdmf_file_name(ucd_header_name, xdmf_dir_file)
! Open the XDMF file to append
      call parallel_write_xdmf_file(xdmf_dir_file, cur_vis_step)
!
      end subroutine parallel_write_xdmf_evo_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_file(xdmf_dir_file, cur_vis_step)
!
      use m_t_step_parameter
      use m_time_data_IO
!
      character(len = kchara), intent(in) :: xdmf_dir_file
      integer(kind=kint), intent(in) :: cur_vis_step
!
      character(len = kchara) :: mesh_dir_file, mesh_file_name
      character(len = kchara) :: field_dir_file, field_file_name
      character(len = kchara) :: node_str, elem_str
      character(len = kchara) :: time_str, attr_str, tmp_str
      integer(kind=kint), parameter :: id_xdmf = 14
      integer :: istep
!
! Only write the file on process 0
      if (my_rank .ne. 0) return
!
      call int_to_str(istack_internod_ucd_list(nprocs), node_str)
      call int_to_str(istack_ele_ucd_list(nprocs), elem_str)
!
! Open the XDMF file to append
      open(id_xdmf, file=xdmf_dir_file, status='old',                   &
     &      position='append', err = 99)
! Back 2 lines
      backspace(id_xdmf)
      backspace(id_xdmf)
      go to 10
!
! Create new XDMF file
  99  continue
      open(id_xdmf, file=xdmf_dir_file, status='replace')
      write(id_xdmf, '(a)') '<?xml version="1.0" ?>'
      write(id_xdmf, '(a)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
      write(id_xdmf, '(a)') '<Xdmf Version="2.0">'
      write(id_xdmf, '(a)') '  <Domain>'
!
  10  continue
!
!
!   Get the mesh file name
      call set_merged_hdf_mesh_file_name                                &
     &   (ucd_header_name, mesh_dir_file)
      call delete_directory_name(mesh_dir_file, mesh_file_name)
!
!  Append field entry
      call real_to_str(time_IO, time_str)
      call set_merged_hdf_field_file_name(ucd_header_name,              &
     &    cur_vis_step, field_dir_file)
      call delete_directory_name(field_dir_file, field_file_name)
      write(id_xdmf, '(2a)')                                            &
     &         '    <Grid Name="CellTime" GridType="Collection" ',      &
     &         'CollectionType="Temporal">'
      write(id_xdmf, '(a)')                                             &
     &         '      <Grid Name="mesh" GridType="Uniform">'
      write(id_xdmf, '(a)')                                             &
     &         '        <Time Value="', trim(time_str), '"/>'
      write(id_xdmf, '(a)')                                             &
     &         '        <Geometry GeometryType="XYZ">'
      write(id_xdmf, '(3a)')                                            &
     &         '          <DataItem Dimensions="', trim(node_str),      &
     &         ' 3" NumberType="Float" Precision="8" Format="HDF">'
      write(id_xdmf, '(3a)')                                            &
     &         '            ', trim(mesh_file_name), ':/nodes'
      write(id_xdmf, '(a)') '          </DataItem>'
      write(id_xdmf, '(a)') '        </Geometry>'
      write(id_xdmf, '(4a)')                                            &
     &         '        <Topology TopologyType="Hexahedron" ',          &
               'NumberOfElements="', trim(elem_str), '">'
!
      call int_to_str(nnod_4_ele_ucd, tmp_str)
      write(id_xdmf, '(4a)')                                            &
     &         '          <DataItem Dimensions="', trim(elem_str),      &
     &         ' ', trim(tmp_str), '" NumberType="UInt" Format="HDF">'
      write(id_xdmf, '(3a)')                                            &
     &         '            ', trim(mesh_file_name), ':/elements'
      write(id_xdmf, '(a)') '          </DataItem>'
      write(id_xdmf, '(a)') '        </Topology>'
!
      do istep = 1, num_field_ucd, 1
        if (num_comp_ucd(istep) .eq. 1) then
          attr_str = "Scalar"
        else if (num_comp_ucd(istep) .eq. 3) then
          attr_str = "Vector"
        else if (num_comp_ucd(istep) .eq. 6) then
          attr_str = "Tensor"
        end if
!
        write(id_xdmf, '(5a)')                                          &
     &         '        <Attribute Name="', trim(phys_name_ucd(istep)), &
     &         '" AttributeType="', trim(attr_str), '" Center="Node">'
        call int_to_str(num_comp_ucd(istep), tmp_str)
        write(id_xdmf, '(5a)')                                          &
     &              '          <DataItem Dimensions="', trim(node_str), &
     &              ' ', trim(tmp_str), '" NumberType="Float" ',        &
                    'Precision="8" Format="HDF">'
        write(id_xdmf,'(4a)') '            ',                           &
     &         trim(field_file_name), ':/', trim(phys_name_ucd(istep))
        write(id_xdmf, '(a)') '          </DataItem>'
        write(id_xdmf, '(a)') '        </Attribute>'
      end do
      write(id_xdmf, '(a)')     '      </Grid>'
      write(id_xdmf, '(a)')     '    </Grid>'
!
      write(id_xdmf, '(a)')         '  </Domain>'
      write(id_xdmf, '(a)')         '</Xdmf>'
      close(id_xdmf)
!
      end subroutine parallel_write_xdmf_file
!
! ----------------------------------------------------------------------
!
      end module hdf5_file_IO
